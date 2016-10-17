Nuages
=======================================================

[![Build Status](https://travis-ci.org/lasp-lang/nuages.svg?branch=master)](https://travis-ci.org/lasp-lang/nuages)

Automated Erlang application deployment for multiple datacenters,
facilitated by Mesosphere's DC/OS.

## Deployment of a stack

To deploy/provision a stack with a given name in a given region, use the
following command.

```
ok = nuages:provision('dcos', 'us-west-2')
```

Stack names must be unique and both the region and stack name must be an
atom.  This command will deploy the `dcos` named stack in the
`us-west-2` region using the Mesosphere DC/OS CloudFormation template.

## Application deployment

```
ok = nuages:deploy("lasp", lasp)
```

This command will deploy the task named "lasp" based on the application
definition provided by the `lasp` module.  This deployment will happen
in all stacks that are currently provisioned.

# Copyright

Copyright 2016 Christopher S. Meiklejohn
