%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_generators).

-include("include/myxi_test.hrl").

%%
%% AMQP
%%

amqp_methods() ->
    ?LET(M, list(amqp_method_record()), M).

amqp_methods_except(Many) when is_list(Many) ->
    Set = gb_sets:from_list(Many),
    ?SUCHTHAT(M, list(amqp_method_record()), not gb_sets:is_member(M, Set));
amqp_methods_except(One) ->
    amqp_methods_except([One]).

amqp_method_record() ->
    union([#'connection.start'{},
           #'connection.start_ok'{},
           #'connection.secure'{},
           #'connection.secure_ok'{},
           #'connection.tune'{},
           #'connection.tune_ok'{},
           #'connection.open'{},
           #'connection.open_ok'{},
           #'connection.close'{},
           #'connection.close_ok'{},
           #'connection.redirect'{},
           #'channel.open'{},
           #'channel.open_ok'{},
           #'channel.flow'{},
           #'channel.flow_ok'{},
           #'channel.close'{},
           #'channel.close_ok'{},
           #'channel.alert'{},
           #'access.request'{},
           #'access.request_ok'{},
           #'exchange.declare'{},
           #'exchange.declare_ok'{},
           #'exchange.delete'{},
           #'exchange.delete_ok'{},
           #'exchange.bind'{},
           #'exchange.bind_ok'{},
           #'exchange.unbind'{},
           #'exchange.unbind_ok'{},
           #'queue.declare'{},
           #'queue.declare_ok'{},
           #'queue.bind'{},
           #'queue.bind_ok'{},
           #'queue.purge'{},
           #'queue.purge_ok'{},
           #'queue.delete'{},
           #'queue.delete_ok'{},
           #'queue.unbind'{},
           #'queue.unbind_ok'{},
           #'basic.qos'{},
           #'basic.qos_ok'{},
           #'basic.consume'{},
           #'basic.consume_ok'{},
           #'basic.cancel'{},
           #'basic.cancel_ok'{},
           #'basic.publish'{},
           #'basic.return'{},
           #'basic.deliver'{},
           #'basic.get'{},
           #'basic.get_ok'{},
           #'basic.get_empty'{},
           #'basic.ack'{},
           #'basic.reject'{},
           #'basic.recover_async'{},
           #'basic.recover'{},
           #'basic.recover_ok'{},
           #'basic.nack'{}]).
