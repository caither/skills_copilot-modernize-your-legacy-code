/**
 * Account Management System
 * Modernized from COBOL legacy system
 *
 * Consolidated to use AccountManager as the single source of business logic.
 * - index.js remains the CLI entry point
 * - AccountManager.js is the reusable library
 */

const readlineSync = require('readline-sync');
const AccountManager = require('./AccountManager');

// Thin adapter to preserve legacy test interfaces while delegating to AccountManager
class DataProgram {
    constructor() {
        this.manager = new AccountManager();
    }
    read() {
        return this.manager.viewBalance();
    }
    write(balance) {
        // Directly set balance to mirror COBOL WRITE behavior
        this.manager.resetBalance(balance);
    }
}

// Thin adapter for Operations that uses AccountManager's formatting logic
class Operations {
    constructor(dataProgram) {
        this.dataProgram = dataProgram;
        // Separate instance for formatting; formatting is stateless
        this.formatter = new AccountManager();
    }
    total() {
        const finalBalance = this.dataProgram.read();
        console.log(`Current balance: ${this.formatter.formatBalance(finalBalance)}`);
    }
    formatBalance(balance) {
        return this.formatter.formatBalance(balance);
    }
}

// Helper: validated numeric input for CLI use
function getAmountInput(prompt) {
    const MAX_AMOUNT = 999999.99;
    let amount;
    let isValid = false;
    do {
        const input = readlineSync.question(prompt);
        const trimmedInput = input.trim();
        if (trimmedInput === '') {
            console.log('Invalid amount format. Please enter a numeric value.');
            continue;
        }
        amount = parseFloat(trimmedInput);
        if (isNaN(amount) || !Number.isFinite(amount)) {
            console.log('Invalid amount format. Please enter a numeric value.');
            continue;
        }
        if (amount < 0 || amount > MAX_AMOUNT) {
            console.log(`Invalid amount. Please enter a positive number between 0 and ${MAX_AMOUNT}.`);
            continue;
        }
        isValid = true;
    } while (!isValid);
    return amount;
}

/**
 * Main Program (CLI) using AccountManager library
 */
class MainProgram {
    constructor() {
        this.account = new AccountManager();
        this.continueFlag = true;
    }
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
    run() {
        while (this.continueFlag) {
            const userChoice = this.displayMenu();
            switch (userChoice) {
                case 1: {
                    const balance = this.account.viewBalance();
                    console.log(`Current balance: ${this.account.formatBalance(balance)}`);
                    break;
                }
                case 2: {
                    const amount = getAmountInput('Enter credit amount: ');
                    const result = this.account.creditAccount(amount);
                    console.log(result.message);
                    break;
                }
                case 3: {
                    const amount = getAmountInput('Enter debit amount: ');
                    const result = this.account.debitAccount(amount);
                    console.log(result.message);
                    break;
                }
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
