# DFS

This is essentially InfluxData's [TICKscript](https://docs.influxdata.com/kapacitor/v1.2/tick/) for Erlang.
Done using leex and yecc.

# Why ?
I love Erlang and always wanted to try leex and yecc for a DSL.

This DSL will be used along with [dataflow](https://github.com/heyoka/dataflow) to build a Timeseries Analytics Framework

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

# License

The MIT License (MIT)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
