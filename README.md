# dbHelpeR

An R package for working with relational databases

`dbHelpeR` is an R package designed to simplify and streamline interactions with SQL databases. It provides high-level functions for managing database metadata, schemas, and other common administrative tasks. Whether you’re a data scientist, database administrator, or analyst, `dbHelpeR` bridges the gap between R and SQL, empowering you to manage and interact with databases effortlessly.


## Features

- **Table comments**: query, create and delete table comments
- **Column comments**: query, create and delete column comments
- **Primary keys**: query, create and delete primary keys
- **Schemas**: query, create and delete schemas
- **User information**: retrieve user specific information e.g. user name or user privileges
- **Database information**: retrieve database specific information e.g. database name or database type

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

## 📄 License

SQLHelper is released under the MIT License, ensuring that it remains free and open for everyone to use. See the [LICENSE](LICENSE) file for more details.
