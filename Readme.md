# Prolexa: Amazon Alexa and Prolog integration #

## Generating intent json for Alexa ##
```
swipl -g "prolexa_intents, halt." prolexa
```
The intents are found in `prolexa_intents.json`.

## Localhost workflow (Docker) ##
To build:
```
docker build . -t prolexa
```

To run:
```
docker run -it -p 4000:4000 prolexa
```

To test the server:
```
curl -v POST http://localhost:4000/prolexa -d @testjson --header "Content-Type: application/json"
```

## Heroku workflow ##
### Initial setup ###
Prerequisites:

- Docker app running in the background
- Installed Heroku CLI (`brew install heroku/brew/heroku`)

---

To see the status of your Heroku webapp use
```
heroku logs
```

in prolexa directory.

---

1. Clone this repository
    ```
    git clone git@github.com:So-Cool/prolexa.git
    cd prolexa
    ```

2. Login to Heroku
    ```
    heroku login
    ```

3. Add Heroku remote
    ```
    heroku git:remote -a prolexa
    ```

### Development workflow ###
1. Before you start open your local copy of Prolexa and login to Heroku
    ```
    cd prolexa
    heroku container:login
    ```

2. Change local files to your liking
3. Once you're done push them to Heroku
    ```
    heroku container:push web
    ```

4. Test your skill and repeat steps *2.* and *3.* if necessary
5. Once you're done commit all the changes and push them to GitHub
    ```
    git commit -am "My commit message"
    git push origin master
    ```
