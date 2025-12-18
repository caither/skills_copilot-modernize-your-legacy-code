/**
 * Student Account Management System
 * Modernized from COBOL legacy system
 * 
 * This application preserves the original business logic:
 * - Initial balance: 1000.00
 * - Prevents overdraft (balance cannot go negative)
 * - Maintains 2 decimal precision for all amounts
 */

const readlineSync = require('readline-sync');

/**
 * Data Layer (equivalent to data.cob)
 * Manages the persistent storage of account balance
 */
class DataProgram {
    constructor() {
        // STORAGE-BALANCE: Initial balance set to 1000.00 as per COBOL spec
        this.storageBalance = 1000.00;
    }

    /**
     * READ operation - retrieves the current balance
     * @returns {number} Current account balance
     */
    read() {
        return this.storageBalance;
    }

    /**
     * WRITE operation - updates the account balance
     * @param {number} balance - New balance to store
     */
    write(balance) {
        this.storageBalance = balance;
    }
}

/**
 * Operations Layer (equivalent to operations.cob)
 * Handles all business logic operations
 */
class Operations {
    constructor(dataProgram) {
        this.dataProgram = dataProgram;
    }

    /**
     * TOTAL operation - displays current balance
     */
    total() {
        const finalBalance = this.dataProgram.read();
        console.log(`Current balance: ${this.formatBalance(finalBalance)}`);
    }

    /**
     * CREDIT operation - adds amount to balance
     */
    credit() {
        const amount = this.getAmountInput('Enter credit amount: ');
        let finalBalance = this.dataProgram.read();
        finalBalance += amount;
        this.dataProgram.write(finalBalance);
        console.log(`Amount credited. New balance: ${this.formatBalance(finalBalance)}`);
    }

    /**
     * DEBIT operation - subtracts amount from balance
     * Includes overdraft protection
     */
    debit() {
        const amount = this.getAmountInput('Enter debit amount: ');
        let finalBalance = this.dataProgram.read();
        
        if (finalBalance >= amount) {
            finalBalance -= amount;
            this.dataProgram.write(finalBalance);
            console.log(`Amount debited. New balance: ${this.formatBalance(finalBalance)}`);
        } else {
            console.log('Insufficient funds for this debit.');
        }
    }

    /**
     * Helper function to get and validate amount input
     * @param {string} prompt - Prompt message for user
     * @returns {number} Validated amount
     */
    getAmountInput(prompt) {
        let amount;
        do {
            const input = readlineSync.question(prompt);
            amount = parseFloat(input);
            if (isNaN(amount) || amount < 0) {
                console.log('Invalid amount. Please enter a positive number.');
                amount = -1;
            }
        } while (amount < 0);
        return amount;
    }

    /**
     * Format balance to match COBOL output format (6 digits + 2 decimals)
     * @param {number} balance - Balance to format
     * @returns {string} Formatted balance
     */
    formatBalance(balance) {
        return balance.toFixed(2).padStart(9, '0');
    }
}

/**
 * Main Program (equivalent to main.cob)
 * Provides the user interface and program flow control
 */
class MainProgram {
    constructor() {
        this.dataProgram = new DataProgram();
        this.operations = new Operations(this.dataProgram);
        this.continueFlag = true;
    }

    /**
     * Display the menu and get user choice
     * @returns {number} User's menu choice
     */
    displayMenu() {
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
        
        const choice = readlineSync.question('Enter your choice (1-4): ');
        return parseInt(choice);
    }

    /**
     * Main logic loop - runs until user chooses to exit
     */
    run() {
        while (this.continueFlag) {
            const userChoice = this.displayMenu();

            switch (userChoice) {
                case 1:
                    this.operations.total();
                    break;
                case 2:
                    this.operations.credit();
                    break;
                case 3:
                    this.operations.debit();
                    break;
                case 4:
                    this.continueFlag = false;
                    break;
                default:
                    console.log('Invalid choice, please select 1-4.');
            }
        }
        console.log('Exiting the program. Goodbye!');
    }
}

// Entry point - start the application
if (require.main === module) {
    const app = new MainProgram();
    app.run();
}

module.exports = { MainProgram, Operations, DataProgram };
