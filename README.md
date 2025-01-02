# dbHelpeR

An R package for working with relational databases

`dbHelpeR` is an R package designed to simplify and streamline interactions with SQL databases. It provides high-level functions for managing database schemas, metadata, and other common administrative tasks. Whether you’re a data scientist, database administrator, or analyst, `dbHelpeR` bridges the gap between R and SQL, empowering you to manage and interact with databases effortlessly.


## Features

- **Table comments**: query, create and delete table comments
- **Column comments**: query, create and delete column comments
- **Primary keys**: query, create and delete primary keys
- **Schemes**: query, create and delete schemes
- **User Information**: retrieve user specific information e.g. user name or user privileges
- 

## Supported Database Types

- PostgreSQL
- Microsoft SQL Server


## Installation

Install `dbHelpeR`

```r
# Install devtools if not already installed
install.packages("devtools")

# Install dbHelpeR from GitHub
devtools::install_github("TobiasKellner/dbHelpeR")
```

## Usage

```r
library(dbHelpeR)

# Connect to your database
```
