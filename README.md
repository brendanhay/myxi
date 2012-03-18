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

<a name="backends" />

**Backends**

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




