[![Build status](https://circleci.com/gh/mtgred/meccg/tree/master.svg?style=shield)](https://circleci.com/gh/mtgred/meccg)

Play Middle-earth:CCG in the browser.

## Live server

http://www.cardnum.net

[Gameplay videos](https://www.youtube.com/results?search_query=cardnum.net)

![screenshot](http://i.imgur.com/xkxOMHc.jpg)


## Card implementation status


[Card rules implementation status](https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml)


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


For more information refer to the [development guide](https://github.com/mtgred/meccg/wiki/Getting-Started-with-Development).

## License

Cardnum.net is released under the [MIT License](http://www.opensource.org/licenses/MIT).
