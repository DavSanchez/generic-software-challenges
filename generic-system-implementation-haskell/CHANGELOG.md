# Revision history for generic-system-challenge

## 0.7.0.0 -- 2022-01-13

* Added documentation. Performed minor refactors for readability.

## 0.6.2.0 -- 2022-01-11

* Applied some refactorings that were apt for increasing test coverage.

## 0.6.1.0 -- 2022-01-10

* Streaming numbers to `numbers.log` now uses the `conduit` library (API is the same).

## 0.6.0.0 -- 2022-01-09

* Now only duplicates get appended to `numbers.log`.
* The checking for duplicates works by maintaining an in-memory `IntSet`.

## 0.5.0.0 -- 2022-01-09

* Streaming input to file `numbers.log`.

## 0.4.0.0 -- 2022-01-07

* Setting a connection limit. If reached, server accepts client, informs it and closes.

## 0.3.0.0 -- 2022-01-06

* Clients can now terminate the server by sending the `terminate` sequence.

## 0.2.0.0 -- 2022-01-05

* Leveraging concurrency. Server, client input processor and reporting done in different threads communicating through channels.
* Client input and reporting pending.

## 0.1.0.0 -- 2022-01-04

* Naive and incomplete, prototype version (cannot be terminated by clients, doesn't remove duplicates).
