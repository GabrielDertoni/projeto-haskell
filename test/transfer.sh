curl \
    -X POST \
    -H 'Accept: application/json' \
    -H 'Content-Type: application/json' \
    -d '{
            "buyer": 1,
            "seller": 2,
            "planet": 10,
            "amount": 100
        }' \
    localhost:8080/transfer

