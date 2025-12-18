/**
 * AccountManager - Node.js implementation of the COBOL Account Management System
 * 
 * This class replicates the behavior of the COBOL programs:
 * - DataProgram: Manages storage balance
 * - Operations: Handles credit/debit operations
 * - MainProgram: Menu-driven interface
 */

class AccountManager {
  constructor(initialBalance = 1000.00) {
    this.balance = initialBalance;
  }

  /**
   * View the current balance
   * @returns {number} Current balance
   */
  viewBalance() {
    return this.balance;
  }

  /**
   * Credit (deposit) amount to the account
   * @param {number} amount - Amount to credit
   * @returns {Object} Result with success flag, message, and new balance
   */
  creditAccount(amount) {
    // Parse the amount to ensure it's a number with 2 decimal places
    const parsedAmount = parseFloat(amount);
    
    // Validate amount
    if (isNaN(parsedAmount)) {
      return {
        success: false,
        message: 'Invalid amount',
        balance: this.balance
      };
    }

    if (parsedAmount < 0) {
      return {
        success: false,
        message: 'Amount cannot be negative',
        balance: this.balance
      };
    }

    // Add amount to balance
    this.balance = parseFloat((this.balance + parsedAmount).toFixed(2));
    
    return {
      success: true,
      message: `Amount credited. New balance: ${this.formatBalance(this.balance)}`,
      balance: this.balance
    };
  }

  /**
   * Debit (withdraw) amount from the account
   * @param {number} amount - Amount to debit
   * @returns {Object} Result with success flag, message, and balance
   */
  debitAccount(amount) {
    // Parse the amount to ensure it's a number with 2 decimal places
    const parsedAmount = parseFloat(amount);
    
    // Validate amount
    if (isNaN(parsedAmount)) {
      return {
        success: false,
        message: 'Invalid amount',
        balance: this.balance
      };
    }

    if (parsedAmount < 0) {
      return {
        success: false,
        message: 'Amount cannot be negative',
        balance: this.balance
      };
    }

    // Check for sufficient funds
    if (this.balance < parsedAmount) {
      return {
        success: false,
        message: 'Insufficient funds for this debit.',
        balance: this.balance
      };
    }

    // Subtract amount from balance
    this.balance = parseFloat((this.balance - parsedAmount).toFixed(2));
    
    return {
      success: true,
      message: `Amount debited. New balance: ${this.formatBalance(this.balance)}`,
      balance: this.balance
    };
  }

  /**
   * Format balance to match COBOL output format (6 digits with 2 decimals)
   * @param {number} balance - Balance to format
   * @returns {string} Formatted balance string
   */
  formatBalance(balance) {
    const balanceStr = balance.toFixed(2);
    const [whole, decimal] = balanceStr.split('.');
    const paddedWhole = whole.padStart(6, '0');
    return `${paddedWhole}.${decimal}`;
  }

  /**
   * Reset balance to initial value
   * @param {number} initialBalance - Balance to reset to
   */
  resetBalance(initialBalance = 1000.00) {
    this.balance = initialBalance;
  }
}

module.exports = AccountManager;
