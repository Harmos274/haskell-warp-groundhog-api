# Haskell Warp / Groundhog API

This project is a test of Haskell web environment, using Warp and Groundhog.

## How to start

You need a PostgreSQL database running with the following:

Database: **db**
User: **postgre**
Paswword: **postgre**

...or you can just modify the `pgConnectionString` defined in the `app/Main.hs` file.

```
$> stack run
Listening on 3000.
```

