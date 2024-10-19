# Create the database

```mysql
-- create the new database
CREATE DATABASE reps_analytics_data;
-- select the created database
USE reps_analytics_data;
```

### Coventry Godiva Harriers data prefix: <code>cdh_<table_name></code>.

## Lookup tables
### Access types
```mysql
CREATE TABLE cgh_access_types (
    access_id int NOT NULL AUTO_INCREMENT,
    description TEXT, 
    PRIMARY KEY(access_id)
);

INSERT INTO cgh_access_types 
    (description)
VALUES ("No access to any application data"), 
       ("Full access to the application"), 
       ("Access to the personal data only"), 
       ("Access to the REPS data only");
```
### Session types
```mysql
CREATE TABLE cgh_session_types (
    session_id int NOT NULL AUTO_INCREMENT,
    description TEXT, 
    PRIMARY KEY(session_id)
);

INSERT INTO cgh_access_types 
    (description)
VALUES ("Just Run"),
       ("Gym");
```

# User data
```mysql
-- Linked to access type
CREATE TABLE cgh_members (
    member_id INT NOT NULL AUTO_INCREMENT,
    first_name VARCHAR(50),
    last_name VARCHAR(50), 
    email VARCHAR(50), 
    phoneNumber VARCHAR(20), 
    location VARCHAR(12), 
    start_date DATE, 
    access_id INT,
    PRIMARY KEY (member_id),
    FOREIGN KEY (access_id) REFERENCES cgh_access_types(access_id)
);
```