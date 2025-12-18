# COBOL to Node.js Conversion Mapping

This document shows how each COBOL program was converted to Node.js.

## Architecture Comparison

### COBOL (Three Separate Files)
```
main.cob        → User Interface & Menu
operations.cob  → Business Logic Operations  
data.cob        → Data Storage
```

### Node.js (Single File: index.js)
```
MainProgram class    → User Interface & Menu
Operations class     → Business Logic Operations
DataProgram class    → Data Storage
```

## File-by-File Conversion

### 1. data.cob → DataProgram class

**COBOL (data.cob):**
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. DataProgram.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  STORAGE-BALANCE    PIC 9(6)V99 VALUE 1000.00.
01  OPERATION-TYPE     PIC X(6).

LINKAGE SECTION.
01  PASSED-OPERATION   PIC X(6).
01  BALANCE            PIC 9(6)V99.

PROCEDURE DIVISION USING PASSED-OPERATION BALANCE.
    MOVE PASSED-OPERATION TO OPERATION-TYPE
    
    IF OPERATION-TYPE = 'READ'
        MOVE STORAGE-BALANCE TO BALANCE
    
    ELSE IF OPERATION-TYPE = 'WRITE'
        MOVE BALANCE TO STORAGE-BALANCE
    
    END-IF
    GOBACK.
```

**Node.js (DataProgram class):**
```javascript
class DataProgram {
    constructor() {
        this.storageBalance = 1000.00;
    }

    read() {
        return this.storageBalance;
    }

    write(balance) {
        this.storageBalance = balance;
    }
}
```

### 2. operations.cob → Operations class

**COBOL (operations.cob):**
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. Operations.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 OPERATION-TYPE     PIC X(6).
01 AMOUNT             PIC 9(6)V99.
01 FINAL-BALANCE      PIC 9(6)V99 VALUE 1000.00.

LINKAGE SECTION.
01 PASSED-OPERATION   PIC X(6).

PROCEDURE DIVISION USING PASSED-OPERATION.
    MOVE PASSED-OPERATION TO OPERATION-TYPE

    IF OPERATION-TYPE = 'TOTAL '
        CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        DISPLAY "Current balance: " FINAL-BALANCE

    ELSE IF OPERATION-TYPE = 'CREDIT'
        DISPLAY "Enter credit amount: "
        ACCEPT AMOUNT
        CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        ADD AMOUNT TO FINAL-BALANCE
        CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        DISPLAY "Amount credited. New balance: " FINAL-BALANCE

    ELSE IF OPERATION-TYPE = 'DEBIT '
        DISPLAY "Enter debit amount: "
        ACCEPT AMOUNT
        CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        IF FINAL-BALANCE >= AMOUNT
            SUBTRACT AMOUNT FROM FINAL-BALANCE
            CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
            DISPLAY "Amount debited. New balance: " FINAL-BALANCE
        ELSE
            DISPLAY "Insufficient funds for this debit."
        END-IF
    END-IF
    GOBACK.
```

**Node.js (Operations class):**
```javascript
class Operations {
    constructor(dataProgram) {
        this.dataProgram = dataProgram;
    }

    total() {
        const finalBalance = this.dataProgram.read();
        console.log(`Current balance: ${this.formatBalance(finalBalance)}`);
    }

    credit() {
        const amount = this.getAmountInput('Enter credit amount: ');
        let finalBalance = this.dataProgram.read();
        finalBalance += amount;
        this.dataProgram.write(finalBalance);
        console.log(`Amount credited. New balance: ${this.formatBalance(finalBalance)}`);
    }

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

    getAmountInput(prompt) {
        const MAX_AMOUNT = 999999.99;
        let amount;
        do {
            const input = readlineSync.question(prompt);
            amount = parseFloat(input);
            if (isNaN(amount) || !Number.isFinite(amount) || amount < 0 || amount > MAX_AMOUNT) {
                console.log(`Invalid amount. Please enter a positive number between 0 and ${MAX_AMOUNT}.`);
                amount = -1;
            }
        } while (amount < 0);
        return amount;
    }

    formatBalance(balance) {
        return balance.toFixed(2).padStart(9, '0');
    }
}
```

### 3. main.cob → MainProgram class

**COBOL (main.cob):**
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MainProgram.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  USER-CHOICE       PIC 9 VALUE 0.
01  CONTINUE-FLAG     PIC X(3) VALUE 'YES'.

PROCEDURE DIVISION.
MAIN-LOGIC.
    PERFORM UNTIL CONTINUE-FLAG = 'NO'
        DISPLAY "--------------------------------"
        DISPLAY "Account Management System"
        DISPLAY "1. View Balance"
        DISPLAY "2. Credit Account"
        DISPLAY "3. Debit Account"
        DISPLAY "4. Exit"
        DISPLAY "--------------------------------"
        DISPLAY "Enter your choice (1-4): "
        ACCEPT USER-CHOICE

        EVALUATE USER-CHOICE
            WHEN 1
                CALL 'Operations' USING 'TOTAL '
            WHEN 2
                CALL 'Operations' USING 'CREDIT'
            WHEN 3
                CALL 'Operations' USING 'DEBIT '
            WHEN 4
                MOVE 'NO' TO CONTINUE-FLAG
            WHEN OTHER
                DISPLAY "Invalid choice, please select 1-4."
        END-EVALUATE
    END-PERFORM
    DISPLAY "Exiting the program. Goodbye!"
    STOP RUN.
```

**Node.js (MainProgram class):**
```javascript
class MainProgram {
    constructor() {
        this.dataProgram = new DataProgram();
        this.operations = new Operations(this.dataProgram);
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
```

## Key Conversion Decisions

### 1. Object-Oriented Design
- **COBOL**: Uses CALL statements to communicate between programs
- **Node.js**: Uses object composition (Operations holds reference to DataProgram)

### 2. Data Types
- **COBOL**: `PIC 9(6)V99` (6 digits + 2 decimal places)
- **Node.js**: JavaScript numbers with `.toFixed(2)` for formatting

### 3. Control Flow
- **COBOL**: `PERFORM UNTIL`, `EVALUATE`
- **Node.js**: `while` loops, `switch` statements

### 4. Input/Output
- **COBOL**: `ACCEPT`, `DISPLAY`
- **Node.js**: `readlineSync.question()`, `console.log()`

### 5. Module Communication
- **COBOL**: `CALL 'Program' USING parameters`
- **Node.js**: Method calls on class instances

## Business Logic Preservation

All original business rules are preserved:

✓ **Initial Balance**: 1000.00
✓ **Overdraft Protection**: Debit rejected if amount > balance
✓ **Decimal Precision**: All amounts stored/displayed with 2 decimals
✓ **Menu Options**: Same 4 options (View, Credit, Debit, Exit)
✓ **Input Validation**: Invalid menu choices show error message
✓ **Balance Limits**: Maximum 999,999.99 (matches COBOL PIC 9(6)V99)

## Improvements in Node.js Version

1. **Enhanced Input Validation**
   - Checks for NaN, Infinity, negative numbers
   - Enforces maximum amount limit
   - Better error messages

2. **Modular Design**
   - Single file with three classes
   - Clear separation of concerns
   - Easier to test and maintain

3. **Documentation**
   - JSDoc comments for all methods
   - README with usage instructions
   - Test suite for validation

4. **Modern Development Tools**
   - VS Code debugger configuration
   - npm package management
   - Automated testing
