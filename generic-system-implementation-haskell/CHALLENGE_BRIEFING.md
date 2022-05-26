# Code Challenge

Using any programming language (taking performance into consideration), write a server ("Application") that opens a socket and restricts input to at most 5 concurrent clients. Clients will connect to the Application and write one or more numbers of 9 digit numbers, each number followed by a server-native newline sequence, and then close the connection. The Application must write a de- duplicated list of these numbers to a log file in no particular order.

## Primary Considerations

- The Application should work correctly as defined below in Requirements.
- The overall structure of the Application should be simple.
- The code of the Application should be descriptive and easy to read, and the build method and runtime parameters must be well-described and work.
- The design should be resilient with regard to data loss.
- The Application should be optimized for maximum throughput, weighed along with the other Primary Considerations and the Requirements below.

## Requirements

1. The Application must accept input from at most 5 concurrent clients on TCP/IP port 4000.
2. Input lines presented to the Application via its socket must either be composed of exactly nine decimal digits (e.g.: `314159265` or `007007009`) immediately followed by a server-native newline sequence; or a termination sequence as detailed in #9, below.
3. Numbers presented to the Application must include leading zeros as necessary to ensure they are each 9 decimal digits.
4. The log file, to be named `numbers.log`, must be created anew and/or cleared when the Application starts.
5. Only numbers may be written to the log file. Each number must be followed by a server-native newline sequence.
6. No duplicate numbers may be written to the log file.
7. Any data that does not conform to a valid line of input should be discarded and the client connection terminated immediately and without comment.
8. Every 10 seconds, the Application must print a report to standard output:

    1. The difference since the last report of the count of new unique numbers that have been received.
    2. The difference since the last report of the count of new duplicate numbers that have been received.
    3. The total number of unique numbers received for this run of the Application.
    4. Example text for #8:

    ```console
    Received 50 unique numbers, 2 duplicates. Unique total: 567231
    ```

9. If any connected client writes a single line with only the word `terminate` followed by a server-native newline sequence, the Application must disconnect all clients and perform a clean shutdown as quickly as possible.
10. Clearly state all of the assumptions you made in completing the Application.

## Notes

- You may write tests at your own discretion. Tests are useful to ensure your Application passes Primary Consideration A.
- You may use common libraries in your project such as Apache Commons and Google Guava, particularly if their use helps improve Application simplicity and readability. However the use of large frameworks, such as Akka, is prohibited.
- Your Application may not for any part of its operation use or require the use of external systems, for example Apache Kafka or Redis.
- At your discretion, leading zeroes present in the input may be stripped—or not used—when writing output to the log or console.
- Robust implementations of the Application typically handle more than 2M numbers per 10-second reporting period on a modern MacBook Pro laptop (e.g.: 16 GiB of RAM and a 2.5 GHz Intel i7 processor).
- To test if your application is working as expected, you can try to telnet to it through the port 4000 by executing:

```console
> telnet localhost 4000
```

And manually type in the numbers sequentially followed by a newline (enter).

## Additional doubts and questions

- In the first paragraph the phrase “The Application must write a de-duplicated list of these numbers to a log file in no particular order”. Should I assume that I get to decide what order the numbers end up in the log file provided there are no duplicates? Or does “no particular order” mean something more like the output should be uniformly random (or at least as random as the input sources)?
  - **Answer:** We don't require or otherwise prefer any ordering in the log file.
  - If the former, could this order be wholly mutated in-file during execution? Or should the log writes be append-only?
    - **Answer:** There is no requirement for the log to be append-only either. However you are free to make it so if you think such property could be useful to have, for example if it leads to simpler code, better performance, etc, or if you think it could make the application more flexible or future-proof.
- In any case, I assume it is not a requirement that partial order (i.e. input from a single client during execution would preserve order within itself, but this order would be then interleaved with the other inputs due to intermediate queuing, input times, etc) is to be guaranteed, right?
  - **Answer:** We don't require or otherwise prefer any ordering in the log file.
- Regarding point 6, should I assume that, for an uniformly random input to the Application, and given infinite time and resources, the (eventually idempotent) end result would be a `numbers.log` file containing all numbers on interval `[0,999999999]`, with no further input being able to alter the file at that point?
  - **Answer:** This is correct, although if we assume a randomly distributed input it will take quite a long time to occur. In any case, we don't expect you to handle that special case. In the event all possible numbers have been received, your application doesn't need to take any special action, and it just needs to avoid writing duplicates to the file as it would during any other point in time.
