-- KUNDER: one row per customer (CPR)
CREATE TABLE IF NOT EXISTS kunder (
    kunde_id    TEXT PRIMARY KEY,      -- CPR (first 10 or 15 chars)
    navn        TEXT NOT NULL,
    adresse     TEXT,
    fodselsdato TEXT                   -- stored as 'DD-MM-YYYY' or 'YYYY-MM-DD'
);

-- BANKER: from Banker.txt
CREATE TABLE IF NOT EXISTS banker (
    reg_nr      TEXT PRIMARY KEY,      -- 4 chars from REG-NR
    banknavn    TEXT NOT NULL,
    bankadresse TEXT,
    telefon     TEXT,
    email       TEXT
);

-- KONTOOPL: from kontoopl copybook
CREATE TABLE IF NOT EXISTS kontoopl (
    kunde_id    TEXT NOT NULL,         -- matches kunder.kunde_id
    konto_id    TEXT NOT NULL,
    konto_type  TEXT,
    balance     REAL,
    valuta_kd   TEXT,
    PRIMARY KEY (kunde_id, konto_id)
);

-- TRANSACTIONS: from Transaktioner.txt
CREATE TABLE IF NOT EXISTS transactions (
    trans_id        INTEGER PRIMARY KEY AUTOINCREMENT,
    cpr             TEXT NOT NULL,     -- CPR from file
    navn            TEXT,
    adresse         TEXT,
    fodselsdato     TEXT,
    konto_id        TEXT,
    reg_nr          TEXT,
    beloeb_dkk      REAL,              -- optional: converted amount in DKK
    beloeb_orig     REAL,              -- original numeric value
    valuta          TEXT,
    trans_type      TEXT,
    butik           TEXT,
    tidsstempel     TEXT,
    FOREIGN KEY (reg_nr)   REFERENCES banker(reg_nr),
    FOREIGN KEY (cpr)      REFERENCES kunder(kunde_id),
    FOREIGN KEY (konto_id) REFERENCES kontoopl(konto_id)
);
