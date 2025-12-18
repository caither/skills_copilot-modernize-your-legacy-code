/**
 * Unit Tests for AccountManager
 * 
 * This test suite mirrors the 40 test cases defined in docs/TESTPLAN.md
 * for the COBOL Account Management System.
 * 
 * Test Categories:
 * - Functional Tests (TC-001 to TC-017)
 * - Boundary Value Tests (TC-018 to TC-025)
 * - Error Handling Tests (TC-026 to TC-034)
 * - Integration Tests (TC-035 to TC-040)
 */

const AccountManager = require('./AccountManager');

describe('AccountManager - Student Account Management System', () => {
  let account;

  beforeEach(() => {
    // Reset account before each test
    account = new AccountManager();
  });

  // ===================================================================
  // 功能測試案例 - FUNCTIONAL TEST CASES
  // ===================================================================

  describe('View Balance Functionality', () => {
    // TC-001: 查看初始餘額
    test('TC-001: View initial balance', () => {
      const balance = account.viewBalance();
      expect(balance).toBe(1000.00);
    });

    // TC-002: 多次查看餘額 (不執行交易)
    test('TC-002: View balance multiple times without transactions', () => {
      const balance1 = account.viewBalance();
      const balance2 = account.viewBalance();
      const balance3 = account.viewBalance();
      
      expect(balance1).toBe(1000.00);
      expect(balance2).toBe(1000.00);
      expect(balance3).toBe(1000.00);
      // Verify balance doesn't change
      expect(balance1).toBe(balance2);
      expect(balance2).toBe(balance3);
    });

    // TC-003: 查看餘額後返回菜單
    test('TC-003: View balance and return to menu (flow control)', () => {
      const balance = account.viewBalance();
      expect(balance).toBe(1000.00);
      
      // Simulate returning to menu and performing another operation
      const result = account.creditAccount(100);
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1100.00);
    });
  });

  describe('Credit/Deposit Functionality - Normal Cases', () => {
    // TC-004: 存入小金額
    test('TC-004: Credit small amount', () => {
      const result = account.creditAccount(100.00);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1100.00);
      expect(result.message).toContain('Amount credited');
      expect(result.message).toContain('001100.00');
    });

    // TC-005: 存入大金額
    test('TC-005: Credit large amount', () => {
      const result = account.creditAccount(50000.00);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(51000.00);
      expect(result.message).toContain('Amount credited');
      expect(result.message).toContain('051000.00');
    });

    // TC-006: 連續存入多次
    test('TC-006: Multiple consecutive credits', () => {
      const result1 = account.creditAccount(500.00);
      expect(result1.success).toBe(true);
      expect(result1.balance).toBe(1500.00);
      
      const result2 = account.creditAccount(300.00);
      expect(result2.success).toBe(true);
      expect(result2.balance).toBe(1800.00);
    });

    // TC-007: 存入金額為小數 (兩位小數)
    test('TC-007: Credit amount with two decimal places', () => {
      const result = account.creditAccount(123.45);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1123.45);
      expect(result.message).toContain('001123.45');
    });

    // TC-008: 存入金額為整數 (無小數)
    test('TC-008: Credit integer amount (no decimals)', () => {
      const result = account.creditAccount(50);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1050.00);
      expect(result.message).toContain('001050.00');
    });
  });

  describe('Debit/Withdrawal Functionality - Success Cases', () => {
    // TC-009: 提取小金額
    test('TC-009: Debit small amount', () => {
      const result = account.debitAccount(100.00);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(900.00);
      expect(result.message).toContain('Amount debited');
      expect(result.message).toContain('000900.00');
    });

    // TC-010: 提取大金額 (但不超出餘額)
    test('TC-010: Debit large amount (within balance)', () => {
      const result = account.debitAccount(999.00);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1.00);
      expect(result.message).toContain('Amount debited');
      expect(result.message).toContain('000001.00');
    });

    // TC-011: 提取金額至餘額為零
    test('TC-011: Debit amount to zero balance', () => {
      const result = account.debitAccount(1000.00);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(0.00);
      expect(result.message).toContain('Amount debited');
      expect(result.message).toContain('000000.00');
    });

    // TC-012: 連續提取多次
    test('TC-012: Multiple consecutive debits', () => {
      const result1 = account.debitAccount(200.00);
      expect(result1.success).toBe(true);
      expect(result1.balance).toBe(800.00);
      
      const result2 = account.debitAccount(300.00);
      expect(result2.success).toBe(true);
      expect(result2.balance).toBe(500.00);
    });

    // TC-013: 提取金額為小數
    test('TC-013: Debit amount with decimals', () => {
      const result = account.debitAccount(50.50);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(949.50);
      expect(result.message).toContain('000949.50');
    });
  });

  describe('Debit/Withdrawal Functionality - Failure Cases (Prevent Overdraft)', () => {
    // TC-014: 提取金額超過餘額
    test('TC-014: Debit amount exceeds balance', () => {
      const result = account.debitAccount(1000.01);
      
      expect(result.success).toBe(false);
      expect(result.balance).toBe(1000.00);
      expect(result.message).toBe('Insufficient funds for this debit.');
    });

    // TC-015: 提取金額遠大於餘額
    test('TC-015: Debit amount far exceeds balance', () => {
      const result = account.debitAccount(50000.00);
      
      expect(result.success).toBe(false);
      expect(result.balance).toBe(1000.00);
      expect(result.message).toBe('Insufficient funds for this debit.');
    });

    // TC-016: 在零餘額時嘗試提取
    test('TC-016: Attempt debit with zero balance', () => {
      // First, debit to zero
      account.debitAccount(1000.00);
      expect(account.viewBalance()).toBe(0.00);
      
      // Then attempt to debit
      const result = account.debitAccount(0.01);
      expect(result.success).toBe(false);
      expect(result.balance).toBe(0.00);
      expect(result.message).toBe('Insufficient funds for this debit.');
    });

    // TC-017: 多次失敗提款不影響餘額
    test('TC-017: Multiple failed debits do not affect balance', () => {
      // First failed debit
      const result1 = account.debitAccount(2000.00);
      expect(result1.success).toBe(false);
      expect(result1.balance).toBe(1000.00);
      
      // Check balance
      expect(account.viewBalance()).toBe(1000.00);
      
      // Second failed debit
      const result2 = account.debitAccount(3000.00);
      expect(result2.success).toBe(false);
      expect(result2.balance).toBe(1000.00);
      
      // Final balance check
      expect(account.viewBalance()).toBe(1000.00);
    });
  });

  // ===================================================================
  // 邊界值測試 - BOUNDARY VALUE TESTS
  // ===================================================================

  describe('Amount Boundary Tests', () => {
    // TC-018: 最小金額 (0.01) 存入
    test('TC-018: Credit minimum amount (0.01)', () => {
      const result = account.creditAccount(0.01);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1000.01);
    });

    // TC-019: 最小金額 (0.01) 提取
    test('TC-019: Debit minimum amount (0.01)', () => {
      const result = account.debitAccount(0.01);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(999.99);
    });

    // TC-020: 零金額存入
    test('TC-020: Credit zero amount', () => {
      const result = account.creditAccount(0.00);
      
      // System accepts transaction but balance doesn't change
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1000.00);
    });

    // TC-021: 零金額提取
    test('TC-021: Debit zero amount', () => {
      const result = account.debitAccount(0.00);
      
      // System accepts transaction but balance doesn't change
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1000.00);
    });

    // TC-022: 最大允許金額存入 (999999.99)
    test('TC-022: Credit maximum allowed amount', () => {
      account = new AccountManager(1.00);
      const result = account.creditAccount(999999.00);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1000000.00);
    });

    // TC-023: 提取金額等於當前餘額
    test('TC-023: Debit amount equal to current balance', () => {
      const result = account.debitAccount(1000.00);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(0.00);
    });
  });

  describe('System State Boundary Tests', () => {
    // TC-024: 應用程式啟動後立即查看餘額
    test('TC-024: View balance immediately after initialization', () => {
      const newAccount = new AccountManager();
      const balance = newAccount.viewBalance();
      
      expect(balance).toBe(1000.00);
    });

    // TC-025: 多個操作的餘額一致性
    test('TC-025: Balance consistency across multiple operations', () => {
      // Credit 200
      account.creditAccount(200.00);
      expect(account.viewBalance()).toBe(1200.00);
      
      // View balance multiple times
      const balance1 = account.viewBalance();
      const balance2 = account.viewBalance();
      
      expect(balance1).toBe(1200.00);
      expect(balance2).toBe(1200.00);
      expect(balance1).toBe(balance2);
    });
  });

  // ===================================================================
  // 錯誤處理與驗證 - ERROR HANDLING AND VALIDATION
  // ===================================================================

  describe('Invalid Input Tests', () => {
    // TC-026 to TC-029 are menu-related and would be tested in integration tests
    // These are not applicable to the AccountManager class unit tests
    
    // TC-030: 金額輸入為負數 (存款)
    test('TC-030: Credit negative amount', () => {
      const result = account.creditAccount(-100.00);
      
      expect(result.success).toBe(false);
      expect(result.balance).toBe(1000.00);
      expect(result.message).toContain('cannot be negative');
    });

    // TC-031: 金額輸入為負數 (提款)
    test('TC-031: Debit negative amount', () => {
      const result = account.debitAccount(-100.00);
      
      expect(result.success).toBe(false);
      expect(result.balance).toBe(1000.00);
      expect(result.message).toContain('cannot be negative');
    });
  });

  describe('Data Precision Tests', () => {
    // TC-032: 小數位數超過兩位 (存款)
    test('TC-032: Credit amount with more than two decimal places', () => {
      const result = account.creditAccount(100.999);
      
      expect(result.success).toBe(true);
      // JavaScript will round/truncate to 2 decimals
      expect(result.balance).toBeCloseTo(1101.00, 2);
    });

    // TC-033: 小數位數超過兩位 (提款)
    test('TC-033: Debit amount with more than two decimal places', () => {
      const result = account.debitAccount(50.555);
      
      expect(result.success).toBe(true);
      // JavaScript will round/truncate to 2 decimals (949.445 rounds to 949.45)
      expect(result.balance).toBe(949.45);
    });

    // TC-034: 連續多次交易後的精度驗證
    test('TC-034: Precision verification after multiple transactions', () => {
      account.creditAccount(0.33);
      account.creditAccount(0.33);
      account.creditAccount(0.33);
      const result = account.debitAccount(0.99);
      
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1000.00);
    });
  });

  // ===================================================================
  // 整合測試 - INTEGRATION TESTS
  // ===================================================================

  describe('Complete Workflow Tests', () => {
    // TC-035: 完整應用程式流程
    test('TC-035: Complete application workflow', () => {
      // Step 1: View initial balance
      expect(account.viewBalance()).toBe(1000.00);
      
      // Step 2: Credit 500.00
      const creditResult = account.creditAccount(500.00);
      expect(creditResult.success).toBe(true);
      
      // Step 3: View new balance
      expect(account.viewBalance()).toBe(1500.00);
      
      // Step 4: Debit 200.00
      const debitResult = account.debitAccount(200.00);
      expect(debitResult.success).toBe(true);
      
      // Step 5: View final balance
      expect(account.viewBalance()).toBe(1300.00);
    });

    // TC-036: 拒絕交易不影響後續操作
    test('TC-036: Rejected transactions do not affect subsequent operations', () => {
      // Step 1: Attempt to debit 2000.00 (should fail)
      const debitResult1 = account.debitAccount(2000.00);
      expect(debitResult1.success).toBe(false);
      
      // Step 2: View balance
      expect(account.viewBalance()).toBe(1000.00);
      
      // Step 3: Credit 100.00
      const creditResult = account.creditAccount(100.00);
      expect(creditResult.success).toBe(true);
      
      // Step 4: View final balance
      expect(account.viewBalance()).toBe(1100.00);
    });

    // TC-037: 長時間應用程式運行穩定性
    test('TC-037: Long-running application stability', () => {
      const operations = [
        { type: 'view', expected: 1000.00 },
        { type: 'credit', amount: 100, expected: 1100.00 },
        { type: 'view', expected: 1100.00 },
        { type: 'debit', amount: 50, expected: 1050.00 },
        { type: 'credit', amount: 200, expected: 1250.00 },
        { type: 'debit', amount: 100, expected: 1150.00 },
        { type: 'view', expected: 1150.00 },
        { type: 'credit', amount: 50, expected: 1200.00 },
        { type: 'debit', amount: 300, expected: 900.00 },
        { type: 'view', expected: 900.00 },
        { type: 'credit', amount: 500, expected: 1400.00 },
        { type: 'debit', amount: 100, expected: 1300.00 },
        { type: 'view', expected: 1300.00 },
        { type: 'credit', amount: 50, expected: 1350.00 },
        { type: 'debit', amount: 50, expected: 1300.00 },
        { type: 'view', expected: 1300.00 },
        { type: 'credit', amount: 100, expected: 1400.00 },
        { type: 'debit', amount: 200, expected: 1200.00 },
        { type: 'view', expected: 1200.00 },
        { type: 'credit', amount: 300, expected: 1500.00 }
      ];

      operations.forEach((op, index) => {
        if (op.type === 'view') {
          expect(account.viewBalance()).toBe(op.expected);
        } else if (op.type === 'credit') {
          const result = account.creditAccount(op.amount);
          expect(result.success).toBe(true);
          expect(result.balance).toBe(op.expected);
        } else if (op.type === 'debit') {
          const result = account.debitAccount(op.amount);
          expect(result.success).toBe(true);
          expect(result.balance).toBe(op.expected);
        }
      });
      
      // Final balance check
      expect(account.viewBalance()).toBe(1500.00);
    });
  });

  describe('System Operation Flow Tests', () => {
    // TC-038: 菜單導航一致性
    test('TC-038: Menu navigation consistency (simulated)', () => {
      // Simulate 5 different operations
      account.viewBalance();
      account.creditAccount(100);
      account.viewBalance();
      account.debitAccount(50);
      account.viewBalance();
      
      // Verify final state is consistent
      expect(account.viewBalance()).toBe(1050.00);
    });

    // TC-039: Exit functionality - Not applicable to AccountManager class
    // This would be tested in the CLI integration tests

    // TC-040: 快速連續操作
    test('TC-040: Rapid consecutive operations', () => {
      // Perform 10 rapid operations
      account.creditAccount(100);  // 1100
      account.debitAccount(50);    // 1050
      account.creditAccount(200);  // 1250
      account.viewBalance();       // 1250
      account.debitAccount(100);   // 1150
      account.creditAccount(50);   // 1200
      account.viewBalance();       // 1200
      account.debitAccount(200);   // 1000
      account.creditAccount(500);  // 1500
      account.viewBalance();       // 1500
      
      // Verify final state
      expect(account.viewBalance()).toBe(1500.00);
    });
  });

  // ===================================================================
  // Additional Edge Cases and Validation Tests
  // ===================================================================

  describe('Additional Validation Tests', () => {
    test('Credit with string number input', () => {
      const result = account.creditAccount('100.50');
      expect(result.success).toBe(true);
      expect(result.balance).toBe(1100.50);
    });

    test('Debit with string number input', () => {
      const result = account.debitAccount('50.25');
      expect(result.success).toBe(true);
      expect(result.balance).toBe(949.75);
    });

    test('Credit with invalid string input', () => {
      const result = account.creditAccount('abc');
      expect(result.success).toBe(false);
      expect(result.message).toBe('Invalid amount');
      expect(result.balance).toBe(1000.00);
    });

    test('Debit with invalid string input', () => {
      const result = account.debitAccount('xyz');
      expect(result.success).toBe(false);
      expect(result.message).toBe('Invalid amount');
      expect(result.balance).toBe(1000.00);
    });

    test('Balance format matches COBOL output', () => {
      account.creditAccount(123.45);
      const balance = account.viewBalance();
      const formatted = account.formatBalance(balance);
      
      expect(formatted).toBe('001123.45');
    });

    test('Reset balance functionality', () => {
      account.creditAccount(500);
      expect(account.viewBalance()).toBe(1500.00);
      
      account.resetBalance();
      expect(account.viewBalance()).toBe(1000.00);
    });

    test('Reset balance with custom initial value', () => {
      account.resetBalance(2000.00);
      expect(account.viewBalance()).toBe(2000.00);
    });
  });
});
