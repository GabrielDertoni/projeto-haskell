curl \
    -X POST \
    -H 'Accept: application/json' \
    -H 'Content-Type: application/json' \
    -d '{
            "name": "John",
            "nickname": "johnoo",
            "email": "john@hotmail.com",
            "birthdate": {
                "day": 2,
                "month": 3,
                "year": 1979
            }
        }' \
    localhost:8080/user
