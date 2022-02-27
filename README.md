# projeto-haskell

## Usage

### Setup

1. [Install postgres](###install-postgres)
2. [Setup postgres to listen on localhost](###setup-postgres-to-listen-on-localhost)
3. [Start postgres](###start-postgres)
4. [Create a `settings.yaml` file](###settings)
5. [Create a database](###creating-databases) with the same name as configured in the config file
6. [Change the postgres user password](###changin-user-password) to match the `settings.yaml` file
7. Compile the code with `stack build` (this may take a while)
8. Run with `stack run`

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

### Entering the postgres command line

By default, postgres will setup a new user called `postgres` which has access to
the postgres command line and other postgres operations. This means that in
order to execute something on the database, this new user will have to be used.
In order to run something as if on the `postgres` user, run `sudo -u postgres
<command>`. To enter the command line, then, run `sudo -u postgres psql`

### Creating databases

Make sure to [be in the postgres command line](###entering-the-postgres-command-line).

```sh
CREATE DATABASE <database name>;
```

### Deleting databases

Make sure to [be in the postgres command line](###entering-the-postgres-command-line).

```sh
DROP DATABASE <database name>;
```

### Changin user password

Make sure to [be in the postgres command line](###entering-the-postgres-command-line).

```sh
ALTER USER postgres PASSWORD '<new password>';
```
