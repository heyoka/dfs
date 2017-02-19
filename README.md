# DFS

This is essentially InfluxData's [TICKscript](https://docs.influxdata.com/kapacitor/v1.2/tick/) for Erlang.
Done using leex and yecc.

# Why ?
I love Erlang and always wanted to try leex and yecc for a DSL.

This DSL will be used along with [dataflow](https://github.com/heyoka/dataflow) to build
an Analytics Framework on top of dataflow computations powered by erlang style message passing.

# Status
In Progress

# Notable Differences from TICKscript
 * For Declarations the keyword 'def' is used instead of 'var'
 * Erlang-Style comments with '%'
 * There is no if statement in Lambda-Expressions, yet
 * Regex Exressions start and end with '?'
 * Built-In Functions (math, erlang and custom modules)

# Examples
Declaration (String Value)

    def inStreamId = '14.987ew349f9e87fwef'
    def outStreamId = '2.404.5dfgs555sa5df5a'
    def dead_message = 'LOW on CARBONIDE !!!'

Declaration (Chain)

    def in1 =
       |stream_in()
        .from(inStreamId)

Use chain Declaration with another Chain Statement

    in1
       |deadman(15m, 30)
        .as(dead_message)

Lambda Expressions Parameter

    in1
      |eval(
        lambda: "value" * "value",
        lambda: 1/"value2",
        .as('value2', 'inv_value2')

    in1
      |where(lambda: "host" == 'server001.example.com'))

Long Chain Statement with multiple Nodes

    |window()
     .every(15s)
     .period(30m)
     .stats(esp_mean, esp_difference)
     .field('val')
     .as('mean', 'diff')
     % write to outstream
     |stream_out(outStreamId)
      .translate(lambda: 5 > "mean")
     |alert()
      .critical(
            % alert when point value is more than 3 stdvs away
            % from running mean
        lambda: sigma("value") > 3, _global
     )
            % alert only when point's timestamp is between these boundaries
      .when(lambda: hour() >= 8 AND hour <= 18)

# Erlang
R16B03 +

# Compile

    rebar3 compile

# Build (Lexer, Parser), Run, Test

    $ make

# License

Apache2
