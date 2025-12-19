# Account Management System

This is a Node.js application modernized from a COBOL legacy system. It provides account management functionality with the following features:

## Features

- **View Balance** - Display current account balance
- **Credit Account** - Add funds to the account (deposit)
- **Debit Account** - Withdraw funds from the account
- **Overdraft Protection** - Prevents balance from going negative
- **Exit** - Close the application

## Business Rules (Preserved from COBOL)

- Initial account balance: **1000.00**
- All amounts are stored with **2 decimal precision**
- **Overdraft protection**: Debit operations are rejected if insufficient funds
- Balance range: **0.00 to 999,999.99**

## Prerequisites

- Node.js (v14 or higher)
- npm (Node Package Manager)

## Installation

1. Change to the accounting directory:
   ```bash
   cd src/accounting
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

## Running the Application

### Method 1: Using npm script
```bash
cd src/accounting
npm start
```

### Method 2: Direct execution
```bash
cd src/accounting
node index.js
```

### Method 3: Using VS Code Debugger
1. Open the project in VS Code
2. Go to Run and Debug (Ctrl+Shift+D)
3. Select "Launch Accounting Application"
4. Press F5 or click the green play button

## Testing

Run the automated test suite:
```bash
cd src/accounting
node test.js
```

Run the interactive simulation test:
```bash
cd src/accounting
node test-interactive.js
```

## Usage Example

```
--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4): 1
Current balance: 001000.00

Enter your choice (1-4): 2
Enter credit amount: 500.00
Amount credited. New balance: 001500.00

Enter your choice (1-4): 3
Enter debit amount: 200.00
Amount debited. New balance: 001300.00

Enter your choice (1-4): 4
Exiting the program. Goodbye!
```

## Architecture

The application follows a three-layer architecture matching the original COBOL design:

### 1. **MainProgram** (main.cob equivalent)
- User interface layer
- Menu display and input handling
- Program flow control

### 2. **Operations** (operations.cob equivalent)
- Business logic layer
- Implements TOTAL, CREDIT, and DEBIT operations
- Validates transactions

### 3. **DataProgram** (data.cob equivalent)
- Data access layer
- Manages account balance storage
- Provides READ and WRITE operations

## Data Flow

```
User Input → MainProgram → Operations → DataProgram → Storage
                ↓              ↓
           Display Menu   Business Logic
                           Validation
```

## Migration Notes

This Node.js application is a direct modernization of the COBOL system with the following equivalences:

| COBOL Program | Node.js Class | Purpose |
|--------------|---------------|---------|
| main.cob | MainProgram | User interface and menu |
| operations.cob | Operations | Business logic operations |
| data.cob | DataProgram | Data storage management |

All original business logic, data integrity rules, and menu options have been preserved.

## License

MIT
