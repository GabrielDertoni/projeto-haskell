# projeto-haskell

## Usage

### Setup

- [projeto-haskell](#projeto-haskell)
  - [Usage](#usage)
    - [Setup](#setup)
    - [Settings](#settings)
  - [Database operations](#database-operations)
    - [Install postgresql](#install-postgresql)
    - [Setup postgres to listen on localhost](#setup-postgres-to-listen-on-localhost)
    - [Start postgres](#start-postgres)
    - [Shut down postgres](#shut-down-postgres)
    - [Install postgresql](#install-postgresql-1)
    - [Setup user](#setup-user)
    - [Setup postgres to listen on localhost](#setup-postgres-to-listen-on-localhost-1)
    - [Entering the postgres command line](#entering-the-postgres-command-line)
    - [Creating databases](#creating-databases)
    - [Deleting databases](#deleting-databases)
    - [Changin user password](#changin-user-password)

### Settings

The `settings.yaml` file will contain settigs on how to connect and access the
database as well as configure other resorces.

```yaml
# settings.yaml

# The port the application will be served on
port: 8080

# Postgres database config.
database:
  user: "postgres"
  password: "12345"
  host: "localhost"
  port: 5432
  name: "stellarium"
  pool-size: 10
```

## Database operations

<details>
<summary>Setup on Ubuntu</summary>

### Install postgresql

```sh
sudo apt update
sudo apt install postgresql postgresql-contrib libpq-dev
```

### Setup postgres to listen on localhost

Edit `/etc/postgresql/12/main/postgresql.conf` and uncomment the line
`listen_address = 'localhost'` by removing the leading `'#'`. Also make sure
that the port is set to `5432`.

### Start postgres

```sh
sudo service postgresql start
```

### Shut down postgres

```sh
sudo service postgresql stop
```

</details>

<details>
<summary>Setup on Arch</summary>

### Install postgresql

Install the `postgresql` package.

### Setup user

```sh
sudo su postgres
initdb -D /var/lib/postgres/data
systemctl start postgresql
createuser --interative
```

Then use the same name as your default user on that machine, and also enable it
to user the superuser.

### Setup postgres to listen on localhost

After exiting the postgres user (simply Control + D out of it).

```sh
sudo cp /usr/share/postgresql/postgresql.conf.sample /var/lib/postgres/postgresql.conf
```

Then edit `/var/lib/postgres/postgresql.conf` and uncomment the line
`listen_address = 'localhost'` by removing the leading `'#'`. Also make sure
that the port is set to `5432`.

</details>

### Entering the postgres command line

By default, postgres will setup a new user called `postgres` which has access to
the postgres command line and other postgres operations. This means that in
order to execute something on the database, this new user will have to be used.
In order to run something as if on the `postgres` user, run `sudo -u postgres
<command>`. To enter the command line, then, run `sudo -u postgres psql`

### Creating databases

Make sure to [be in the postgres command line](#entering-the-postgres-command-line).

```sh
CREATE DATABASE <database name>;
```

### Deleting databases

Make sure to [be in the postgres command line](#entering-the-postgres-command-line).

```sh
DROP DATABASE <database name>;
```

### Changin user password

Make sure to [be in the postgres command line](#entering-the-postgres-command-line).

```sh
ALTER USER postgres PASSWORD '<new password>';
```
