# SNMP Quick Start

**Authors**: Vance Shipley and Serge Aleynikov

## Agent Configuration

### Directories
In the following examples we will assume we are logged in as the user otpuser and working from a home directory of otpuser. Create a subdirectory to contain the configuration files of the agent named snmp/agent/conf and another for the working files named snmp/agent/db.

Code listing 2.1: Create the Agent Configuration and Database Directories
```bash
$ mkdir -p snmp/agent/conf
$ mkdir -p snmp/agent/db
```
## Agent Information
The snmp/agent/conf/agent.conf file defines the information for this agent. The IP address and port which the agent will respond to must be provided as well as a unique identifier called an engineID. The maximum size of a packet must also be provided.

#### Code listing 2.2: Create the Agent Information File
```bash
$ cat > snmp/agent/conf/agent.conf
{intAgentIpAddress, [127,0,0,1]}.
{intAgentUDPPort, 4000}.
{snmpEngineID, "agent's engine"}.
{snmpEngineMaxMessageSize, 484}.
^D
```
Note: Control-D to terminate

## System Information

The snmp/agent/conf/standard.conf file defines the information for the system being managed. Here we will use the sysObjectID for the Ericsson OTP application. If you are creating an agent to manage your own embedded system you may want to apply for a private enterprise number assignment and create your own sysObjectID for your network element.

#### Code listing 2.3: Create the System Information File
```bash
$ cat > snmp/agent/conf/standard.conf
{sysName, "SNMP Quick Start HowTo Demo"}.
{sysDescr, "Erlang/OTP Agent"}.
{sysContact, "vances@motivity.ca"}.
{sysLocation, "5th Floor machine room, rack 21A"}.
{sysObjectID, [3,6,1,4,1,193,19]}.  % {ericsson otp}
{sysServices, 72}.
{snmpEnableAuthenTraps, enabled}.
^D
$
```
## Community
The snmp/agent/conf/community.conf file defines the communities for the system being managed. It is required for SNMPv1 or SNMPv2c.

#### Code listing 2.4: Create the Community File
```bash
$ cat > snmp/agent/conf/community.conf
{"public", "public", "initial", "", ""}.
^D
$
```
## MIB Views
The snmp/agent/conf/vacm.conf file defines the views for VACM. This determines what access rights users have to areas of the MIBS. Here we will define access rights for the community defined above.

#### Code listing 2.5: Create the VACM File
```bash
$ cat > snmp/agent/conf/vacm.conf
{vacmSecurityToGroup, v2c, "initial", "initial"}.
{vacmSecurityToGroup, usm, "initial", "initial"}.
{vacmAccess, "initial", "", any, noAuthNoPriv, exact, "restricted", "", "restricted"}.
{vacmAccess, "initial", "", usm, authNoPriv, exact, "internet", "internet", "internet"}.
{vacmAccess, "initial", "", usm, authPriv, exact, "internet", "internet", "internet"}.
{vacmViewTreeFamily, "internet", [1,3,6,1], included, null}.
{vacmViewTreeFamily, "restricted", [1,3,6,1], included, null}.
^D
$
```

## Application Environment
Create the snmp/agent.config system configuration file to be used when we start the node to run the agent. Here we define the required application environment variables. The paths to the subdirectories we created earlier are given so the agent application can find it's configuration files and persistent database.

#### Code listing 2.6: Create the Agent OTP System Configuration File
```bash
$ cat > snmp/agent.config
[{snmp,
        [{agent,
            [{config, [{dir, "snmp/agent/conf/"}]},
            {db_dir, "snmp/agent/db/"}]}]}].
^D
$
```

## Running the Agent
The configuration in snmp/agent.config will be used when we start the emulator with the -config snmp/agent commandline option.

#### Code listing 3.1: Starting the Agent for the First Time
```erlang
$ erl -sname agent -config snmp/agent

Eshell V5.9  (abort with ^G)
(agent@myhost)1> application:start(snmp).

=ERROR REPORT==== 10-Feb-2012::15:15:31 ===
** Configuration error: [FRAMEWORK-MIB]: missing context.conf file => generating a default file

=INFO REPORT==== 10-Feb-2012::15:15:31 ===
[ snmp : agent : snmp_user_based_sm_mib : <0.45.0> ] 
USM: Incomplete configuration. Generating empty usm.conf.
ok
```
Note: The system will create some missing files for us.

The agent is now running using SNMPv1, SNMPv2c and SNMPv3.

## Verifying Configuration

#### Code listing 3.2: Get the System Name
```erlang
(agent@myhost)2> {value, OID} = snmpa:name_to_oid(sysName).
{value,[1,3,6,1,2,1,1,5]}
(agent@myhost)2> % We need to add a '0' to the OID to access the value
(agent@myhost)3> snmpa:get(snmp_master_agent, [OID ++ [0]]).
["SNMP Quick Start HowTo Demo"]
```

## Manager Configuration

**Directories**

Create a subdirectory to contain the configuration files of the manager named snmp/manager/conf and another for the working files named snmp/manager/db.

#### Code listing 4.1: Create the Manager Configuration and Database Directories
```erlang
$ mkdir -p snmp/manager/conf
$ mkdir -p snmp/manager/db
```

## Manager Information
The snmp/manager/conf/manager.conf file defines the information for this manager. The IP address and port which the manager will use must be provided as well as a unique identifier called an engineID. The maximum size of a packet must also be provided.

#### Code listing 4.2: Create the Manager Information File
```bash
$ cat > snmp/manager/conf/manager.conf
{port, 5000}.
{address, [127,0,0,1]}.
{engine_id, "manager's engine"}.
{max_message_size, 484}.
^D
$
```
### Users
The snmp/manager/conf/users.conf file defines the manager users. A manager user is implemented in a callback module. Here we will define a simple manager user using the default module.

#### Code listing 4.3: Create the Users File
```bash
$ cat > snmp/manager/conf/users.conf
{"simple_user", snmpm_user_default, undefined}.
^D
$
```

### Agents
The snmp/manager/conf/agents.conf file defines the agents the manager will use. Here we'll define one agent; the one we started above. For now we'll define it to use SNMPv2c.

#### Code listing 4.4: Create the Agents File
```erlang
$ cat > snmp/manager/conf/agents.conf
{"simple_user", "otp agent", "public", [127,0,0,1], 4000, "agent's engine",
    infinity, 484, v2, v2c, "initial", noAuthNoPriv}.
^D
$
```

## Application Environment
Create the snmp/manager.config system configuration file to be used when we start the node to run the manager. Here we define the required application environment variables. The paths to the subdirectories we created earlier are given so the manager application can find it's configuration files and persistent database.

#### Code listing 4.5: Create the Manager OTP System Configuration File
```bash
$ cat > snmp/manager.config
[{snmp,
        [{manager,
            [{config, [{dir, "snmp/manager/conf/"},
                {db_dir, "snmp/manager/db/"}]}]}]}].
^D
$
```

## Running the Manager

**Startup**

The configuration in snmp/manager.config will be used when we start the emulator with the -config snmp/manager commandline option.

#### Code listing 5.1: Starting the Manager for the First Time
```erlang
$ erl -sname manager -config snmp/manager

Eshell V5.9  (abort with ^G)
(manager@myhost)1> application:start(snmp).
ok
Verifying Configuration
Code listing 5.2: List Users

(manager@myhost)2> snmpm:which_users().
["simple_user"]
Code listing 5.3: List Agents

(manager@myhost)3> snmpm:which_agents().
["otp agent"]
Code listing 5.4: Verify Agent Configuration

(manager@myhost)5> snmpm:agent_info("otp agent", community).
{ok,"public"}
(manager@myhost)6> snmpm:agent_info("otp agent", engine_id).
{ok,"agent's engine"}
```

## Query the Agent
Here we will send an SNMP query to the (possibly remote) agent and return the result. We will make the request using the OID for sysName as above.

#### Code listing 5.5: Get System Name from Agent
```erlang
(manager@myhost)7> snmpm:sync_get("simple_user", "otp agent", [[1,3,6,1,2,1,1,5,0]]).
{ok,{noError,0,
             [#varbind{oid = [1,3,6,1,2,1,1,5,0],
                       variabletype = 'OCTET STRING',
                       value = "SNMP Quick Start HowTo Demo",org_index = 1}]},
    4992}
```
Note: In the above example we have previously used the shell command rr/1 to load the record definitions so that the output is parsed with record field names. E.g. rr("lib/erlang/lib/snmp-*/include/*").

## Adding Authentication and Privacy
User Based Security Model (USM)
With SNMPv3 the agent and manager may share a secret for authentication using either MD5 or SHA. RFC 2274 specifies an algorithm to generate a localized key using a passphrase and engineID. This localized authentication key is defined in the agent's and manager's usm.conf file.

## Generating Localized Keys
The snmp application can be used to generate a localized key.

#### Code listing 6.1: Generating a Localized Authentication Key for the Agent
```erlang
$ erl

Eshell V5.9  (abort with ^G)
1> application:start(crypto).
ok
2> snmp:passwd2localized_key(md5, "The quick brown fox jumps over the lazy dog.", "agent's engine").
[40,165,209,16,245,231,199,157,53,56,248,82,228,181,160,143]
When authentication is used it is also possible to ensure privacy of the transmitted information. A shared secret is used to encrypt the scopedPDU using DES.

Code listing 6.2: Generating a Localized Privacy Key for the Agent

4> snmp:passwd2localized_key(md5, "Pack my box with five dozen liquor jugs.", "agent's engine").
[4,11,162,206,20,118,62,83,28,129,124,10,66,5,100,136]
Security Data Configuration
The snmp/agent/conf/usm.conf file defines the security data for each user of an agent. Here we have chosen MD5 based authentication and DES encryption privacy.

Code listing 6.3: Create the USM Configuration File for the Agent

$ cat > snmp/agent/conf/usm.conf
{"agent's engine", "simple_user", "initial", zeroDotZero,
    usmHMACMD5AuthProtocol, "", "",
    usmDESPrivProtocol, "", "", "",
    [40,165,209,16,245,231,199,157,53,56,248,82,228,181,160,143],
    [4,11,162,206,20,118,62,83,28,129,124,10,66,5,100,136]}.
^D
$
```
The snmp/manager/conf/usm.conf file defines the security data for each manager user on each agent.

#### Code listing 6.4: Create the USM Configuration File for the Manager
```bash
$ cat > snmp/manager/conf/usm.conf
{"agent's engine", "simple_user", "initial",
    usmHMACMD5AuthProtocol, [40,165,209,16,245,231,199,157,53,56,248,82,228,181,160,143],
    usmDESPrivProtocol, [4,11,162,206,20,118,62,83,28,129,124,10,66,5,100,136]}.
^D
$
```

## Agent Configuration
Earlier we configured the manager to use SNMPv2c to contact the agent. Here we'll replace that with an SNMPv3 USM configuration using MD5 authentication and DES privacy.

#### Code listing 6.5: Create the Agents Configuration File for the Manager
```bash
$ cat > snmp/manager/conf/agents.conf
{"simple_user", "otp agent", "public", [127,0,0,1], 4000, "agent's engine",
    infinity, 484, v3, usm, "initial", authPriv}.
^D
$
```
## Start the Application
Note: The crypto application is required for authentication/privacy.

#### Code listing 6.6: Start the Agent
```erlang
$ erl -sname agent -config snmp/agent

Eshell V5.9  (abort with ^G)
(agent@myhost)1> application:start(crypto).
ok
(agent@myhost)2> application:start(snmp).
ok
Code listing 6.7: Load the User Table

(agent@myhost)3>  Dir = code:priv_dir(snmp) ++ "/mibs/",
(agent@myhost)3>  snmpa:load_mibs([Dir ++ "SNMP-USER-BASED-SM-MIB"]).
ok
(agent@myhost)4>  snmp_user_based_sm_mib:configure("snmp/agent/conf").
Note: You may add the snmp application environment variable {mibs, ["<path-to-snmp>/priv/mibs/SNMP-USER-BASED-SM-MIB"]} to the agent.config file to automatically load on start.
```

#### Code listing 6.8: Start the Manager

$ erl -sname manager -config snmp/manager
```erlang
Eshell V5.9  (abort with ^G)
(manager@myhost)1> application:start(crypto).
ok
(manager@myhost)2> application:start(snmp).
ok
```
#### Code listing 6.9: Querying the Agent
```erlang
(manager@myhost)3> snmpm:load_mib(code:priv_dir(snmp) ++ "/mibs/SNMPv2-MIB").
ok
(manager@myhost)4> snmpm:sync_get("simple_user", "otp agent", [[sysName,0]]).

=INFO REPORT==== 10-Feb-2012::22:31:31 ===
SNMPM default user callback received handle_inform:
   TargetName:   "otp agent"
   SnmpReport: {noError,0,
                        [{varbind,[1,3,6,1,6,3,15,1,1,2,0],'Counter32',0,1}]}
   UserData:   undefined
{error,{timeout,1305755172}}
(manager@myhost)5> snmpm:sync_get("simple_user", "otp agent", [[1,3,6,1,2,1,1,5,0]]).
{ok,{noError,0,
             [#varbind{oid = [1,3,6,1,2,1,1,5,0],
                       variabletype = 'OCTET STRING',
                       value = "SNMP Quick Start HowTo Demo",org_index = 1}]},
    4632}
```
Note: The initial query does not contain the correct snmpEngineBoots and snmpEngineTime values and so is discarded by the agent. Subsequent queries will be properly authenticated.


## Using the OTP MIBs

### Agent

Note: The otp_mib instrumentation uses mnesia.

#### Code listing 7.1: Starting the Agent
```erlang
(agent@myhost)1> application:start(mnesia).
ok
(agent@myhost)2> application:start(snmp).
ok
Code listing 7.2: Loading the OTP MIB

(agent@myhost)3> otp_mib:load(snmp_master_agent).
ok
```

### Manager

#### Code listing 7.3: Starting the Manager
```erlang
(manager@myhost)1> application:start(snmp).
ok
```
Here we use the OID for otp.erlNodeName.1 to query the nodename of the agent's Erlang/OTP node.

#### Code listing 7.4: Querying the Agent
```erlang
(manager@myhost)2> snmpm:load_mib(code:priv_dir(otp_mibs) ++ "/mibs/OTP-MIB").
ok
(manager@myhost)3> snmpm:sync_get("simple_user", "agent's engine", [[erlNodeName,1]]).                                          
{ok,{noError,0,
             [#varbind{oid = [1,3,6,1,4,1,193,19,3,1,2,1,1,1,2,1],
                       variabletype = 'OCTET STRING',value = "agent@myhost",
                       org_index = 1}]},
    4926}
```
### Adding the os_mon Application
If we run the os_mon application more info is available.

#### Code listing 7.5: Starting os_mon
```erlang
(agent@myhost)4> application:start(sasl).
ok
(agent@myhost)5> application:start(os_mon).
ok
(agent@myhost)6> os_mon_mib:load(snmp_master_agent).
ok
```
Note: SASL progress reports were hidden for our example.

### Walking the OTP MIB
To demonstrate the information managed with the OTP-MIB I've used the net-snmp application from a Unix shell prompt to walk the OTP MIB.

#### Code listing 7.6: Using net-snmp to walk the agent's OTP MIB
```erlang
$ snmpwalk -c public -v2c 127.0.0.1:4000 otp
OTP-MIB::erlNodeName.1 = STRING: agent@myhost
OTP-MIB::erlNodeMachine.1 = STRING: BEAM
OTP-MIB::erlNodeVersion.1 = STRING: 5.9
OTP-MIB::erlNodeRunQueue.1 = Gauge32: 0
OTP-MIB::erlNodeRunTime.1 = Counter32: 430
OTP-MIB::erlNodeWallClock.1 = Counter32: 112176
OTP-MIB::erlNodeReductions.1 = Counter32: 1329646
OTP-MIB::erlNodeProcesses.1 = Gauge32: 84
OTP-MIB::erlNodeInBytes.1 = Counter32: 7958884
OTP-MIB::erlNodeOutBytes.1 = Counter32: 1684165
OTP-MIB::applName.1.1 = STRING: otp_mibs
OTP-MIB::applName.1.2 = STRING: os_mon
OTP-MIB::applName.1.3 = STRING: snmp
OTP-MIB::applName.1.4 = STRING: mnesia
OTP-MIB::applName.1.5 = STRING: sasl
OTP-MIB::applName.1.6 = STRING: stdlib
OTP-MIB::applName.1.7 = STRING: kernel
OTP-MIB::applDescr.1.1 = STRING: SNMP managment information base for Erlang/OTP nodes.
OTP-MIB::applDescr.1.2 = STRING: CPO  CXC 138 46
OTP-MIB::applDescr.1.3 = STRING: SNMP  CXC 138 13
OTP-MIB::applDescr.1.4 = STRING: MNESIA  CXC 138 12
OTP-MIB::applDescr.1.5 = STRING: SASL  CXC 138 11
OTP-MIB::applDescr.1.6 = STRING: ERTS  CXC 138 10
OTP-MIB::applDescr.1.7 = STRING: ERTS  CXC 138 10
OTP-MIB::applVsn.1.1 = STRING: 1.0.7
OTP-MIB::applVsn.1.2 = STRING: 2.2.8
OTP-MIB::applVsn.1.3 = STRING: 4.21.4
OTP-MIB::applVsn.1.4 = STRING: 4.6
OTP-MIB::applVsn.1.5 = STRING: 2.2
OTP-MIB::applVsn.1.6 = STRING: 1.18
OTP-MIB::applVsn.1.7 = STRING: 2.15
OTP-OS-MON-MIB::loadMemorySystemWatermark.0 = INTEGER: 80
OTP-OS-MON-MIB::loadMemoryErlProcWatermark.0 = INTEGER: 5
OTP-OS-MON-MIB::loadSystemTotalMemory."agent@myhost" = Gauge32: 3045288000 bytes
OTP-OS-MON-MIB::loadSystemUsedMemory."agent@myhost" = Gauge32: 2129992000 bytes
OTP-OS-MON-MIB::loadLargestErlProcess."agent@myhost" = STRING: <0.6.0>
OTP-OS-MON-MIB::loadLargestErlProcessUsedMemory."agent@myhost" = Gauge32: 142328 bytes
OTP-OS-MON-MIB::loadCpuLoad."agent@myhost" = INTEGER: 19
OTP-OS-MON-MIB::loadCpuLoad5."agent@myhost" = INTEGER: 22
OTP-OS-MON-MIB::loadCpuLoad15."agent@myhost" = INTEGER: 23
OTP-OS-MON-MIB::loadOsWordsize."agent@myhost" = Gauge32: 32
OTP-OS-MON-MIB::loadSystemTotalMemory64."agent@myhost" = Counter64: 3045288000 bytes
OTP-OS-MON-MIB::loadSystemUsedMemory64."agent@myhost" = Counter64: 2129992000 bytes
OTP-OS-MON-MIB::loadLargestErlProcessUsedMemory64."agent@myhost" = Counter64: 142328 bytes
OTP-OS-MON-MIB::diskAlmostFullThreshold.0 = INTEGER: 80
OTP-OS-MON-MIB::diskDescr.1.1 = STRING: /
OTP-OS-MON-MIB::diskKBytes.1.1 = Gauge32: 243862672 kbytes
OTP-OS-MON-MIB::diskCapacity.1.1 = INTEGER: 79
```

## Sending SNMP traps
Traps and Notifications
The snmp/agent/conf/notify.conf file contains information about SNMP trap definitions.

#### Code listing 8.1: Create the Notify File
```bash
$ cat > snmp/agent/conf/notify.conf
{"MyFirstCoolTrap", "tag1", trap}.
^D
$
```
The snmp/agent/conf/target_addr.conf file contains information about SNMP Target Address Definitions. Note that TagList can contain multiple Tags separated by spaces.

#### Code listing 8.2: Create the Target Address File
```bash
$ cat > snmp/agent/conf/target_addr.conf
{"MyFirstCoolTrap", [127,0,0,1], 162, 5000, 3, "tag1", "MyCoolTrapParams", "agent's engine"}.
^D
$
```
The snmp/agent/conf/target_params.conf file contains information about SNMP Target Parameters Definitions.

#### Code listing 8.3: Create the Target Parameters File
```bash
$ cat > snmp/agent/conf/target_params.conf
{"MyCoolTrapParams", v2c, v2c, "initial", noAuthNoPriv}.
^D
$
```
### Creating a MIB file containing a trap definition
Now it's time to create a MIB file that defines a test trap. While it is possible to reference traps in existing MIB files, we are going to beef up this tutorial by creating a sample MIB.

#### Code listing 8.4: Create a sample MIB file with a trap definition
```erlang
$ mkdir -p snmp/agent/mibs
$ cat > snmp/agent/mibs/MY-TRAP-MIB.mib
MY-TRAP-MIB DEFINITIONS ::= BEGIN

IMPORTS
    MODULE-IDENTITY, NOTIFICATION-TYPE
        FROM SNMPv2-SMI
    NOTIFICATION-GROUP
        FROM SNMPv2-CONF
    sysContact, sysName, sysLocation
        FROM SNMPv2-MIB
    otpModules, otpApplications
        FROM OTP-REG
    ;

 testMIBModule MODULE-IDENTITY
       LAST-UPDATED "200506100000Z"
       ORGANIZATION "IDT"
       CONTACT-INFO "Serge Aleynikov (serge@hq.idt.net)"
       DESCRIPTION  "Experimental MIB module."
       ::= { otpModules 10 }

testMIB        OBJECT IDENTIFIER ::= { otpApplications 1000 }
testMIBConformance
               OBJECT IDENTIFIER ::= { testMIB 1 }
testMIBTraps   OBJECT IDENTIFIER ::= { testMIB 2 }

testMIBMyFirstCoolTrap NOTIFICATION-TYPE
    OBJECTS {
        sysContact,
        sysName,
        sysLocation
        }
    STATUS  current
    DESCRIPTION
        "This event is sent when an OTP user is desperately
         trying to get the SNMP traps working."
    ::= { testMIBTraps 1 }

testMIBTrapGroups
                OBJECT IDENTIFIER ::= { testMIBConformance 2 }

testMIBTrapGroup NOTIFICATION-GROUP
    NOTIFICATIONS { testMIBMyFirstCoolTrap }
    STATUS        current
    DESCRIPTION
        "The notification which is generated from EVA."
    ::= { testMIBTrapGroups 4 }

END
^D
$
```
Let's start a trap daemon so that we can verify that traps are getting sent

#### Code listing 8.5: Start a snmptrapd daemon
```erlang
$ sudo snmptrapd -P -F "%02.2h:%02.2j TRAP%w.%q from %A\n  %v\n%" -m "snmp/agent/mibs/MY-TRAP-MIB.mib"
```
Once this is done, we are ready to compile our MIB and send a sample trap

#### Code listing 8.6: Create a sample MIB file with a trap definition
```erlang
(agent@myhost)7> snmpc:compile("snmp/agent/mibs/MY-TRAP-MIB", [{il, ["otp_mibs/priv/mibs/"]}]).
{ok, "snmp/agent/mibs/MY-TRAP-MIB.bin"}
(agent@myhost)7> snmp:load_mibs(snmp_master_agent, ["/path/to/.../snmp/agent/mibs/MY-TRAP-MIB"]).
ok
(agent@myhost)8> snmpa:send_notification(snmp_master_agent, testMIBMyFirstCoolTrap, no_receiver, "DrpManager", "", []).
{send_trap,testMIBMyFirstCoolTrap,"DrpManager",[],no_receiver,[]}
```
The following output appears in the snmptrapd terminal window

#### Code listing 8.7: SNMP trap received
```
2005-06-10 16:12:27 UCD-snmp version 4.2.4 Started.
16:12 TRAP0.0 from 0.0.0.0
  system.sysUpTime.0 = Timeticks: (92244) 0:15:22.44
  .iso.org.dod.internet.snmpV2.snmpModules.snmpMIB.snmpMIBObjects.snmpTrap.snmpTrapOID.0 =
      OID: enterprises.ericsson.otp.otpApplications.testMIB.testMIBTraps.testMIBMyFirstCoolTrap
  system.sysLocation.0 = 5th Floor machine room, rack 21A
  system.sysName.0 = SNMP Quick Start HowTo Demo
  system.sysContact.0 = serge@hq.idt.net
```
