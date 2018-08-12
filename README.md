[![Build status](https://circleci.com/gh/rezwits/cardnum/tree/master.svg?style=shield)](https://circleci.com/gh/rezwits/cardnum/tree/master)

Play Middle-earth:CCG in the browser.

## Live server

http://www.cardnum.net

[Ch 1 - Basics](https://www.youtube.com/watch?v=NVUjf6YrL_A)

[Ch 2 - Games](https://www.youtube.com/watch?v=kMNXLibDVGE)

[Ch 3 - Drafting](https://www.youtube.com/watch?v=r5tFzhFe6-E)

[Ch 4 - Resources](https://www.youtube.com/watch?v=3-DuGG2NNAc)

[Ch 5 - Hazards](https://www.youtube.com/watch?v=cy8lLzPUBlE)

[Ch 6 - Item Tips](https://www.youtube.com/watch?v=QCURuej2IsY)

![screenshot](/resources/public/img/meccg_shot2.jpg)

## Card implementation status

[Card rules implementation status](https://docs.google.com/spreadsheets/d/1Ly2RVe4QZRhN6TUfV1YO9DuuYvywzMnnaCunQapzzfs/edit?usp=sharing)


## Dependencies

* [Leiningen](https://leiningen.org/) (version 2+)
* [MongoDB](https://docs.mongodb.com/manual/administration/install-community/)


## Installation

Install frontend dependencies:

```
$ npm install -g bower
$ npm install -g stylus
$ lein deps
$ bower install
```

Launch MongoDB and fetch card data:

```
$ mongod --dbpath data
```
or on windows
```
$ mongod --dbpath .\data\
```
then:
```
$ lein fetch
```

Compile and watch client side ClojureScript files<sup>[1](#footnote_1)</sup>:

```
$ lein figwheel
```

Launch web server:

* As a REPL process (recommended for development):
    ```
    $ lein repl
    ```
* As a standalone process in production mode (must first run `lein uberjar` and `lein cljsbuild once prod`):
    ```
    $ java -jar target/netrunner-standalone.jar
    ```

Open http://localhost:1042/


## Tests (unimplemented)

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


<a name="footnote_1">1</a>: This is only necessary the first time you run the project, or if you are working on front end changes.
