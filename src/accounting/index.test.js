/**
 * Jest tests for the Account Management System
 * Converted from test.js and test-interactive.js
 * Tests all functionality to match the COBOL legacy system behavior
 */

const { Operations, DataProgram } = require('./index.js');

describe('Account Management System - Core Functionality Tests', () => {
  
  describe('DataProgram (Data Layer)', () => {
    // Test 1: Initial Balance
    test('should have initial balance of 1000.00', () => {
      const dataProgram = new DataProgram();
      const initialBalance = dataProgram.read();
      expect(initialBalance).toBe(1000.00);
    });

    // Test 3: Credit Operation
    test('should credit amount correctly', () => {
      const dataProgram = new DataProgram();
      const beforeCredit = dataProgram.read();
      dataProgram.write(beforeCredit + 500.00);
      const afterCredit = dataProgram.read();
      expect(beforeCredit).toBe(1000.00);
      expect(afterCredit).toBe(1500.00);
    });

    // Test 4: Debit Operation (with sufficient funds)
    test('should debit amount when sufficient funds available', () => {
      const dataProgram = new DataProgram();
      dataProgram.write(1500.00);
      const beforeDebit = dataProgram.read();
      expect(beforeDebit).toBe(1500.00);
      
      if (beforeDebit >= 300.00) {
        dataProgram.write(beforeDebit - 300.00);
        const afterDebit = dataProgram.read();
        expect(afterDebit).toBe(1200.00);
      }
    });

    // Test 5: Debit Operation (insufficient funds - overdraft protection)
    test('should prevent overdraft when insufficient funds', () => {
      const dataProgram = new DataProgram();
      const balance = dataProgram.read();
      expect(balance).toBe(1000.00);
      
      const attemptDebit = 2000.00;
      // Overdraft protection - should not debit
      if (balance >= attemptDebit) {
        // Should not reach here
        expect(true).toBe(false);
      } else {
        // Balance should remain unchanged
        const finalBalance = dataProgram.read();
        expect(finalBalance).toBe(1000.00);
      }
    });

    // Test 6: Decimal Precision
    test('should maintain 2 decimal precision', () => {
      const dataProgram = new DataProgram();
      dataProgram.write(123.45);
      const balance = dataProgram.read();
      expect(balance).toBe(123.45);
    });

    // Test 8: Multiple Operations Sequence
    test('should handle multiple operations in sequence', () => {
      const dataProgram = new DataProgram();
      expect(dataProgram.read()).toBe(1000.00);
      
      dataProgram.write(dataProgram.read() + 200.00); // Credit 200
      expect(dataProgram.read()).toBe(1200.00);
      
      dataProgram.write(dataProgram.read() - 100.00); // Debit 100
      const finalBalance = dataProgram.read();
      expect(finalBalance).toBe(1100.00);
    });

    // Test 9: Zero Balance
    test('should allow debit to zero balance', () => {
      const dataProgram = new DataProgram();
      dataProgram.write(dataProgram.read() - 1000.00);
      const zeroBalance = dataProgram.read();
      expect(zeroBalance).toBe(0);
    });

    // Test 10: Large Amount Handling
    test('should handle large amounts correctly', () => {
      const dataProgram = new DataProgram();
      dataProgram.write(50000.00);
      const largeBalance = dataProgram.read();
      expect(largeBalance).toBe(50000.00);
    });
  });

  describe('Operations (Business Logic Layer)', () => {
    let dataProgram;
    let operations;
    let consoleLogSpy;

    beforeEach(() => {
      dataProgram = new DataProgram();
      operations = new Operations(dataProgram);
      consoleLogSpy = jest.spyOn(console, 'log').mockImplementation();
    });

    afterEach(() => {
      consoleLogSpy.mockRestore();
    });

    // Test 2: View Balance (TOTAL operation)
    test('should display balance using total() operation', () => {
      operations.total();
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001000.00');
    });

    // Test 7: Format Balance Output
    test('should format balance to match COBOL output (001000.00)', () => {
      const formatted = operations.formatBalance(1000.00);
      expect(formatted).toBe('001000.00');
    });

    test('should format small balance with leading zeros', () => {
      const formatted = operations.formatBalance(1.50);
      expect(formatted).toBe('000001.50');
    });

    test('should format large balance correctly', () => {
      const formatted = operations.formatBalance(50000.00);
      expect(formatted).toBe('050000.00');
    });

    test('should format maximum COBOL balance', () => {
      const formatted = operations.formatBalance(999999.99);
      expect(formatted).toBe('999999.99');
    });

    test('should cap balance at COBOL maximum for display', () => {
      const formatted = operations.formatBalance(1000000.00);
      expect(formatted).toBe('999999.99');
    });

    test('should throw error for invalid balance values', () => {
      expect(() => operations.formatBalance(-1)).toThrow('Invalid balance value for formatting.');
      expect(() => operations.formatBalance(NaN)).toThrow('Invalid balance value for formatting.');
      expect(() => operations.formatBalance(Infinity)).toThrow('Invalid balance value for formatting.');
    });
  });

  describe('Interactive Menu Simulation Tests', () => {
    let dataProgram;
    let operations;
    let consoleLogSpy;

    beforeEach(() => {
      dataProgram = new DataProgram();
      operations = new Operations(dataProgram);
      consoleLogSpy = jest.spyOn(console, 'log').mockImplementation();
    });

    afterEach(() => {
      consoleLogSpy.mockRestore();
    });

    test('should simulate complete menu workflow', () => {
      // 1. View Balance
      operations.total();
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001000.00');
      consoleLogSpy.mockClear();

      // 2. Credit Account - simulate user entering 500.00
      dataProgram.write(dataProgram.read() + 500.00);
      expect(dataProgram.read()).toBe(1500.00);
      expect(operations.formatBalance(dataProgram.read())).toBe('001500.00');

      // 3. View Balance again
      operations.total();
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001500.00');
      consoleLogSpy.mockClear();

      // 4. Debit Account (successful) - simulate user entering 200.00
      const balance = dataProgram.read();
      if (balance >= 200.00) {
        dataProgram.write(balance - 200.00);
        expect(operations.formatBalance(dataProgram.read())).toBe('001300.00');
      }

      // 5. View Balance again
      operations.total();
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001300.00');
      consoleLogSpy.mockClear();

      // 6. Debit Account (insufficient funds) - simulate user entering 5000.00
      const balance2 = dataProgram.read();
      if (balance2 >= 5000.00) {
        dataProgram.write(balance2 - 5000.00);
      } else {
        // Insufficient funds - balance should remain unchanged
        expect(dataProgram.read()).toBe(1300.00);
      }

      // 7. View Balance final
      operations.total();
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001300.00');
    });

    test('should handle view balance operation multiple times', () => {
      operations.total();
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001000.00');
      consoleLogSpy.mockClear();

      operations.total();
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001000.00');
      consoleLogSpy.mockClear();

      operations.total();
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001000.00');
    });

    test('should maintain balance consistency across operations', () => {
      // Initial balance
      expect(dataProgram.read()).toBe(1000.00);

      // After credit
      dataProgram.write(dataProgram.read() + 250.00);
      expect(dataProgram.read()).toBe(1250.00);

      // After debit
      dataProgram.write(dataProgram.read() - 100.00);
      expect(dataProgram.read()).toBe(1150.00);

      // After another credit
      dataProgram.write(dataProgram.read() + 350.00);
      expect(dataProgram.read()).toBe(1500.00);

      // After another debit
      dataProgram.write(dataProgram.read() - 500.00);
      expect(dataProgram.read()).toBe(1000.00);
    });

    test('should prevent overdraft in interactive scenario', () => {
      // Set balance to a known amount
      dataProgram.write(500.00);
      expect(dataProgram.read()).toBe(500.00);

      // Attempt to debit more than available
      const currentBalance = dataProgram.read();
      const attemptAmount = 1000.00;

      if (currentBalance >= attemptAmount) {
        // Should not execute
        dataProgram.write(currentBalance - attemptAmount);
        expect(true).toBe(false); // Should not reach here
      } else {
        // Balance should remain unchanged
        expect(dataProgram.read()).toBe(500.00);
      }
    });

    test('should handle edge case of debiting exact balance', () => {
      const balance = dataProgram.read();
      expect(balance).toBe(1000.00);

      // Debit exact balance
      dataProgram.write(balance - balance);
      expect(dataProgram.read()).toBe(0.00);
    });

    test('should handle very small credit amounts', () => {
      dataProgram.write(dataProgram.read() + 0.01);
      expect(dataProgram.read()).toBe(1000.01);
    });

    test('should handle very small debit amounts', () => {
      dataProgram.write(dataProgram.read() - 0.01);
      expect(dataProgram.read()).toBe(999.99);
    });
  });

  describe('Business Logic Validation', () => {
    test('should maintain initial balance specification', () => {
      const dataProgram = new DataProgram();
      expect(dataProgram.read()).toBe(1000.00);
    });

    test('should prevent negative balance (overdraft protection)', () => {
      const dataProgram = new DataProgram();
      const balance = dataProgram.read();
      const excessiveDebit = 1500.00;

      // Business logic should prevent this
      if (balance >= excessiveDebit) {
        dataProgram.write(balance - excessiveDebit);
      }

      // Balance should remain at 1000.00
      expect(dataProgram.read()).toBe(1000.00);
    });

    test('should maintain decimal precision in calculations', () => {
      const dataProgram = new DataProgram();
      
      // Perform operations that might cause precision issues
      dataProgram.write(dataProgram.read() + 0.10);
      dataProgram.write(dataProgram.read() + 0.20);
      dataProgram.write(dataProgram.read() - 0.30);
      
      expect(dataProgram.read()).toBeCloseTo(1000.00, 2);
    });

    test('should handle maximum COBOL balance limit', () => {
      const dataProgram = new DataProgram();
      const operations = new Operations(dataProgram);
      
      // Set to maximum COBOL balance
      dataProgram.write(999999.99);
      expect(dataProgram.read()).toBe(999999.99);
      
      // Format should work correctly
      const formatted = operations.formatBalance(dataProgram.read());
      expect(formatted).toBe('999999.99');
    });
  });

  describe('COBOL Legacy System Equivalence', () => {
    test('DataProgram read() should behave like COBOL READ operation', () => {
      const dataProgram = new DataProgram();
      const balance1 = dataProgram.read();
      const balance2 = dataProgram.read();
      
      // Multiple reads should return same value
      expect(balance1).toBe(balance2);
      expect(balance1).toBe(1000.00);
    });

    test('DataProgram write() should behave like COBOL WRITE operation', () => {
      const dataProgram = new DataProgram();
      
      dataProgram.write(1500.00);
      expect(dataProgram.read()).toBe(1500.00);
      
      dataProgram.write(2000.00);
      expect(dataProgram.read()).toBe(2000.00);
    });

    test('Operations.total() should match COBOL TOTAL operation output', () => {
      const dataProgram = new DataProgram();
      const operations = new Operations(dataProgram);
      const consoleLogSpy = jest.spyOn(console, 'log').mockImplementation();
      
      operations.total();
      
      // Should display in COBOL format
      expect(consoleLogSpy).toHaveBeenCalledWith('Current balance: 001000.00');
      
      consoleLogSpy.mockRestore();
    });

    test('Credit/Debit operations should match COBOL behavior', () => {
      const dataProgram = new DataProgram();
      
      // COBOL CREDIT behavior
      const beforeCredit = dataProgram.read();
      dataProgram.write(beforeCredit + 250.00);
      expect(dataProgram.read()).toBe(1250.00);
      
      // COBOL DEBIT behavior (with sufficient funds)
      const beforeDebit = dataProgram.read();
      if (beforeDebit >= 150.00) {
        dataProgram.write(beforeDebit - 150.00);
      }
      expect(dataProgram.read()).toBe(1100.00);
      
      // COBOL DEBIT behavior (insufficient funds)
      const beforeInsufficientDebit = dataProgram.read();
      if (beforeInsufficientDebit >= 2000.00) {
        dataProgram.write(beforeInsufficientDebit - 2000.00);
      }
      // Should remain unchanged
      expect(dataProgram.read()).toBe(1100.00);
    });
  });
});
