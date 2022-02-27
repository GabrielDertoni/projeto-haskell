# Stellarium NFT Provider



Welcome to the future! Here we have a prototype for a non-blockchain
planet-based NFT system. Interstellar trade begins NOW!

- [Stellarium NFT Provider](#stellarium-nft-provider)
  - [API](#api)
    - [Objects](#objects)
    - [Enums](#enums)
    - [User API](#user-api)
    - [Planet API](#planet-api)
    - [Transactions API](#transactions-api)
  - [Setup](#setup)
    - [Settings](#settings)
    - [Database](#database)
      - [Usage](#usage)

## API

Each entry is formatted as:

```raw
METHOD ENDPOINT -> [responseA: returnA; responseB: returnB; ...]
```

### Objects

And we have the following objects:

- Date:

  Field   | Type  | Description | Example
  :-      | :-    | :-          | :-
  `day`   | Int   | day         | `"day": 1`
  `month` | Int   | month       | `"month": 2`
  `year`  | Int   | year        | `"year": 3`

- Planet:

  Field         | Type        | Description       | Example
  :-            | :-          | :-                | :-
  `name`        | String      | Planet name       | `"name": "Jupiter"`
  `mass`        | Int         | Planet mass       | `"mass": 1`
  `type`        | Planet type | Planet type       | `"type": "gaseous"`
  `disttoearth` | Int         | Distance to Earth | `"disttoearth": 1`
  `radius`      | Int         | Planet radius     | `"radius": 1`
  `ico`         | Float       | Planet valuation  | `"ico": 1.0`
  `ownerId`     | Int         | Owner ID          | `"ownerId": 1`

### Enums

- Planet type:

  Type      | Description
  :-        | :-
  Gaseous   | Gaseous planet
  Rocky     | Rocky planet
  EarthLike | Earth like planet

### User API

- Add user

  > Use this to add a create a new user account

  `POST /user -> 201: User ID`

  Field       | Type    | Description         | Example
  :-          | :-      | :-                  | :-
  `name`      | String  | User name           | `"name": "John Doe"`
  `nickname`  | String  | User nickname       | `"nickname": "johnny"`
  `email`     | String  | User email          | `"email": "john@email.com"`
  `birthdate` | Date    | User birthday date  | `"birtdate": {"day": 11, "month": 9, "year": 1987}`

- Get user account info

  `GET /user/{user-id} -> [200: User JSON; 404: None]`

- Update user account info

  > Fixup user account info

  `PATCH /user/{user-id} -> 204: None`

  Field       | Type    | Description         | Example
  :-          | :-      | :-                  | :-
  `name`      | String  | User name           | `"name": "John Doe"`
  `nickname`  | String  | User nickname       | `"nickname": "johnny"`
  `email`     | String  | User email          | `"email": "john@email.com"

- Delete user account

  `DELETE /user/{user-id} -> 204: None`

- List user accounts

  `GET /userList -> 200: [User JSON]`

### Planet API

- Discover a new planet

  > This is how a user can mine planets into their collection

  `POST /discoverPlanet -> 200: {Planet JSON, Planet ID}`

  Field     | Type  | Description | Example
  :-        | :-    | :-          | :-
  `userId`  | Int   | User ID     | `"userId": 1`

- List planet info

  `GET /planet/{planet-id} -> [200: {Planet JSON, Planet ID}; 404: None]`

- Patch planet

  > Fixup planet info

  `PATCH /planet/{planet-id} -> 204: None`

  Field         | Type        | Description       | Example
  :-            | :-          | :-                | :-
  `name`        | String      | Planet name       | `"name": ""`
  `mass`        | Int         | Planet mass       | `"mass": 1`
  `type`        | Planet type | Planet type       | `"type": "gaseous"`
  `disttoearth` | Int         | Distance to Earth | `"disttoearth": 1`
  `radius`      | Int         | Planet radius     | `"radius": 1`

- Delete planet

  `DELETE /planet/{planet-id} -> 204: None`

- List planets by owner

  `GET /userPlanets/{user-id} -> 200: [{Planet JSON, Planet ID}]`

- List all planets

  `GET /planetList -> 200: {Planet JSON, Planet ID}`

### Transactions API

- Transfer a planet

  `POST /transfer -> [201: Transaction ID; 406: Message]`

  Field     | Type  | Description         | Example
  :-        | :-    | :-                  | :-
  `buyer`   | Int   | Buyer ID            | `"buyer": 1`
  `seller`  | Int   | Seller ID           | `"seller": 2`
  `planet`  | Int   | Planet ID           | `"planet": 1`
  `amount`  | Float | Transaction Amount  | `"amount": 123.1`

- Sell planet

  > This is how a user can gain currency without trading a planet
  >
  > The planet is sold to the "bank", which means it's lost
  >
  > The value of a planet is it's ICO

  `POST /sellPlanet -> [200: Transaction ID; 404: Message]`

  Field       | Type  | Description | Example
  :-          | :-    | :-          | :-
  `userId`    | Int   | User ID     | `"userId": 1`
  `planetId`  | Int   | Planet ID   | `"planetId": 1`

- Get transaction info

  `GET /transaction/{transaction-id} -> [200: User JSON; 404: None]`

- Delete user

  `DELETE /transaction/{transaction-id} -> 204: None`

## Setup

This section shouldn't be necessary if you only want to access the API.

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

### Database

<details>
<summary>On Ubuntu</summary>

1. Install postgresql

  ```sh
  sudo apt update
  sudo apt install postgresql postgresql-contrib libpq-dev
  ```

2. Setup postgres to listen on localhost

  Edit `/etc/postgresql/12/main/postgresql.conf` and uncomment the line
  `listen_address = 'localhost'` by removing the leading `'#'`. Also make sure
  that the port is set to `5432`.

3. Start postgres

  ```sh
  sudo service postgresql start
  ```

> To shut postgres down, use
>
>  ```sh
>  sudo service postgresql stop
>  ```

</details>

<details>
<summary>On Arch</summary>

1. Install postgresql

  Install the `postgresql` package.

2. Setup user

  ```sh
  sudo su postgres
  initdb -D /var/lib/postgres/data
  systemctl start postgresql
  createuser --interative
  ```

  Then use the same name as your default user on that machine, and also enable it
  to user the superuser.

3. Setup postgres to listen on localhost

  After exiting the postgres user (simply Control + D out of it).

  ```sh
  sudo cp /usr/share/postgresql/postgresql.conf.sample /var/lib/postgres/postgresql.conf
  ```

  Then edit `/var/lib/postgres/postgresql.conf` and uncomment the line
  `listen_address = 'localhost'` by removing the leading `'#'`. Also make sure
  that the port is set to `5432`.

</details>

#### Usage

By default, postgres will setup a new user called `postgres` which has access to
the postgres command line and other postgres operations. This means that in
order to execute something on the database, this new user will have to be used.
In order to run something as if on the `postgres` user, run `sudo -u postgres
<command>`. To enter the command line, then, run `sudo -u postgres psql`

1. Creating databases

  Make sure to [be in the postgres command line](#usage).

  ```sh
  CREATE DATABASE <database name>;
  ```

2. Deleting databases

  Make sure to [be in the postgres command line](#usage).

  ```sh
  DROP DATABASE <database name>;
  ```

3. Changing user password

  Make sure to [be in the postgres command line](#usage).

  ```sh
  ALTER USER postgres PASSWORD '<new password>';
  ```
