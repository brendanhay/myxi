Myxomatosis
===========

[![Build Status](https://secure.travis-ci.org/brendanhay/myxi.png)](http://travis-ci.org/brendanhay/myxi)

Table of Contents
-----------------

* [Introduction](#introduction)
* [Features](#features)
* [Scenarios](#scenarios)
* [Build](#build)
* [Configure](#configure)
    * [Frontends](#frontends)
    * [Backends](#backends)
* [Contribute](#contribute)


<a name="introduction" />

Introduction
------------

WIP


<a name="features" />

Features
------------

WIP


<a name="scenarios" />

Scenarios
------------

WIP


<a name="build" />

Build
------------

WIP


<a name="configure" />

Configure
------------

TCP keepalive packets can be sent from myxi to the connected client and server sockets.
See: [gen_tcp](erlang.org/doc/man/gen_tcp.html)

```erlang
{tcp, [
    {keepalive, true}
]}
```

myxi supports [statsd](github.com/etsy/statsd) integration. The url for the `statsd`
instance and the `graphite` namespace prefix are configurable:

```erlang
{statsd, [
   {namespace, "graphite.namespace"},
   {url, 'ENVIRONMENT_VARIABLE'}
]}
```

<a name="frontends" />

**Frontends**

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
from the `AMQP` handshake to select a backend.


<a name="backends" />

**Backends**

Backends in myxi consist of 3 main parts:

`balancer`: is a running instance of the `myxi_balancer` behaviour which performs
balancing based on the implementation configured in `{balancer, ...}`.

`middleware`: is any number of implementations of the `myxi_middleware` behaviour.
Middleware is wrapped (left->right), and can potentially perform
 pre-hooks, post-callbacks, or modifications of the `AMQP` methods as they are forwarded
from client->server.

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


<a name="middleware" />

Middleware
------------

WIP


<a name="contribute" />

Contribute
------------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/myxi/issues).




