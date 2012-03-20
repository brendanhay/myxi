Myxomatosis
===========

[![Build Status](https://secure.travis-ci.org/brendanhay/myxi.png)](http://travis-ci.org/brendanhay/myxi)

Table of Contents
-----------------

* [Introduction](#introduction)
* [Features](#features)
* [Build](#build)
* [Testing](#testing)
* [Configure](#configure)
    * [Frontends](#frontends)
    * [Backends](#backends)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="introduction" />

Introduction
------------

Myxamatosis or myxi for short, is designed to be an AMQP aware load-balancer and proxy for RabbitMQ.

It is currently at a prototype stage.

<a name="features" />

Features
--------

* Route traffic to clusters or individual nodes based on AMQP username
* Transparently set all queues as mirrored
* Automatic federation of backends based on exchange locality
* Statsd integration


<a name="build" />

Build
-----

```shell
make
make boot
```


<a name="testing" />

Testing
-------

Myxi has a growing number of Unit, Integration, and Property Based Tests. There are a number of Makefile targets which are used to run the respective suites.

```shell
make test        # All
make unit        # Unit + Property tests only
make integration # Integration suites
```

Both `unit` and `integration` targets support an `ENV` variable `T` specifying a specific suite to run. For example:

```shell
make unit T=myxi_util            # The eunit module, minus the '_test.erl' suffix
make integration T=myxi_frontend # A common_test suite, minus the '_SUITE.erl' suffix
```


<a name="configure" />

Configure
---------

TCP keepalive packets can be sent from myxi to the connected client and server sockets.
See: [gen_tcp](erlang.org/doc/man/gen_tcp.html)

```erlang
{tcp, [
    {keepalive, true}
]}
```

Any other tcp options supported by `gen_tcp` can be added here.

Myxi supports [statsd](github.com/etsy/statsd) integration. The url for the `statsd`
instance and the `graphite` namespace prefix are configurable:

```erlang
{statsd, [
   {namespace, "graphite.namespace"},
   {url, 'ENVIRONMENT_VARIABLE'}
]}
```

<a name="frontends" />

#### Frontends

A frontend consists of the configuration for an acceptor pool and listen socket,
and a routing mechanism to determine which backend+balancer accepted connections will be directed to.

```erlang
{frontends, [
    [{ip, "0.0.0.0"},
     {port, 5672},
     {max, 10},
     {router, myxi_user_router, [
         [{user, <<"rabbit">>}, {backend, rabbit}],
         [{user, <<"chinchilla">>}, {backend, chinchilla}]
     ]}]
]},
```

Currently only `myxi_user_router` is supported, which uses the `LOGIN` component
from the AMQP handshake to select a backend.


<a name="backends" />

#### Backends

Backends in myxi consist of 3 main parts:

**balancer** is a running instance of the `myxi_balancer` behaviour which performs
balancing based on the implementation configured in `{balancer, ...}`.

Currently only a very simple round-robin based algorithm is available.

**middleware** is any number of implementations of the `myxi_middleware` behaviour.
Middleware is wrapped (left->right), and can potentially perform pre-hooks, post-callbacks, or modifications of the AMQP methods as they are forwarded from client->server.

**nodes** is a list of RabbitMQ node names and the port they are accepting AMQP connections on. Since RabbitMQ runs Distributed Erlang using `-sname`, the listed nodes must also
confirm to short node names.

```erlang
{backends, [
    {rabbit, [
        {balancer, myxi_roundrobin_balancer},
        {middleware, [
            myxi_topology_middleware,
            myxi_federation_middleware,
            myxi_ha_middleware
        ]},
        {nodes, [
            [{node, 'rabbit@13inches'},
             {port,  5673}]
        ]}
    ]},

    {chinchilla, [
        {balancer, myxi_roundrobin_balancer},
        {middleware, [
            myxi_topology_middleware,
            myxi_federation_middleware,
            myxi_ha_middleware
        ]},
        {nodes, [
            [{node, 'chinchilla@13inches'},
             {port, 5674}]
        ]}
    ]}
]}
```

#### Available Middleware

**myxi_topology_middleware** builds an in memory topology map (currently only exchanges),
of AMQP resources and their backend locations.

As backend nodes come up (at boot, or after being unavailable), their resources are mapped via `rpc:call`.
`exchange.delcare` methods are passed through the proxy their success is verified asynchronously and
added to the map.

**myxi_federation_middleware** ensures that calls to `queue.bind` for an exchange which
exists on a different physical backend will succeed.

A federation exchange will be created on the current node with the upstream being
the backend on which the exchange actually lives. This requires some convention
regarding configuration, see: (https://github.com/brendanhay/myxi/tree/master/dev) for an example.

**myxi_ha_middleware** ensures clients need no knowledge about the backend HA setup
by ensuring all `queue.declare` method calls have `x-ha-policy=all` appended to their arguments.

The plan is to extend this to a percentage or some other sane/non-all value in the future.


<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/myxi/issues).


<a name="licence" />

Licence
-------

Myxi is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)