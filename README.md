# issue-wanted: a prototype
Web application to help beginners to start contributing into Haskell projects


This is a basic prototype of the [IssueWanted](https://github.com/kowainik/issue-wanted/) project.

# Demonstration
Now it looks pretty poor, but functionally it resembles the structure of the expected app. It downloads the data from github into database and shows it using rest api.




# Project structure
* `startDatabase.sh` runs PostgreSQL within docker. 
* `init.sql` creates necessary tables.
* `app/Server.hs` is an rest api server, I use Scotty to handle the queries.
* `app/Fetcher.hs` fetches the data from githubAPI and store it into the database. It only initializes the database for now, but it's supposed to fetch all the updates and keep the database in actual state.
* `frontend/app.elm` is a very basic frontend in Elm (see Demonstration)


# Build instruction
### Run the database
For convenience I use docker to start and initialize the PostgreSQL
* Download docker and run the daemon (`dockerd`)
* Run `./startDatabase.sh`
### Build the fetcher and the server
* `stack build`
### Run the fetcher
It fetches data from github and store it into the database
* `stack exec fetcher`
### Start the server
* `stack exec server`
It starts the api server in `localhost:1234`
### Build the frontend
* install elm (using npm)
* `elm make frontend/app.elm` creates `index.html`


