

Play Middle-earth:CCG in the browser.

## Live server

http://www.cardnum.net


![screenshot](/resources/public/img/meccg_shot2.jpg)

## Card implementation status


[Card rules implementation status](https://docs.google.com/spreadsheets/d/1Ly2RVe4QZRhN6TUfV1YO9DuuYvywzMnnaCunQapzzfs/edit?usp=sharing)


## Dependencies

* Node.js, Node Package Manager
* Leiningen (version 2+)
* MongoDB
* Zero MQ


## Installation

Install frontend dependencies:

```
$ npm install
```

Launch MongoDB and fetch card data:

```
$ mongod
$ npm run fetch
```

Compile and watch client side Clojurescript files:

```
$ lein figwheel
```

Compile server side Clojure files:

```
$ lein uberjar
```

Launch game server:

```
$ java -jar target/meccg-standalone.jar
```

Launch the Node server:

```
$ npm start
```

## Tests

To run all tests:

```
$ lein test test.all
```

To run a single test file:
```
$ lein test test.cards.agendas
```


For more information refer to the [development guide](https://github.com/rezwits/meccg/wiki/Getting-Started-with-Development).

## License

Cardnum.net is released under the [MIT License](http://www.opensource.org/licenses/MIT).
