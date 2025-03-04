-record(bad_request, {
    reason :: term()
}).

-record(missing_query_parameter, {
    parameter :: binary()
}).

-record(malformed_query_parameter, {
    parameter :: binary(),
    error :: term()
}).

-record(missing_header, {
    header :: binary()
}).
