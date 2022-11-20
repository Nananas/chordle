CREATE TABLE page_events (
    id SERIAL PRIMARY KEY,
    page TEXT NOT NULL,
    event TEXT NOT NULL
);


CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    username TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    email TEXT NOT NULL,
    date_added INTEGER NOT NULL
);