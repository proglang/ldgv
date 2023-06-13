# How to run LDGVNW
Using LDGVNW is a little more difficult than simply starting a single program. In addition to the requirements of LDGV, you also need a network connected via IPv4. Should this network span more than one device, the IP-addresses within the LDGVNW programs need to be altered to reflect this architecture. The current version of LDGVNW was tested on Fedora 37 and macOS 13.2 and should work on every recent Linux or macOS machine, it is unknown whether LDGVNW works on Windows machines.

To run a LDGVNW example, found in the networking-examples folder, each program in the example folder needs to be run. To start the "handoff" example, you would run the following commands in different terminals or on different machines:

- `stack run ldgv -- interpret networking-examples/handoff/client.ldgvnw`
- `stack run ldgv -- interpret networking-examples/handoff/server.ldgvnw`
- `stack run ldgv -- interpret networking-examples/handoff/handoff.ldgvnw`

The order in which these commands are executed is not relevant.

To test all the different test-cases in an easier way, they can be automatically run, with the scrips provided in the networking-tests folder. Simply running one of these scripts will execute the whole test-case at once.
The testNW\* scripts contain all the tests, except for the recursion test.


# An Introduction to LDGVNW
LDGVNW adds two new commands to LDGV to allow for networking capabilities:

- `accept <Own Port> <Own Type>`
- `connect <Own Port> <Own Type> <Partner Address> <Partner Port>`

The accept command requires an integer, as a port for others to connect, and a type, that will be required of a connecting connection.
Once a communication partner connects with a desired type, the accept command will return a VChan, of this type.
The connect command also requires an integer port and a desired type, but also needs to specify a string, for the address of the connection partner, and an integer, for the port of the connection partner.
Just like with the accept command, the connect command will return a VChan, of the desired type, once a connection has been established. 
Important to note is that, with the current implementation, only IPv4 addresses are supported.
IPv6 and Unix domain sockets could be supported in the future, with relatively low effort.

# The Logical Communication Architecture
## Messages and Responses
In LDGVNW, there are 7 possible Messages and 5 possible Responses.
The messages are:

- `Introduce <UserID> <Own Port> <Type Name> <Type Structure>`
- `NewValue <UserID> <Value Index> <Value>`
- `RequestValue <UserID> <Value Index>`
- `AcknowledgeValue <UserID> <Value Index>`
- `NewPartnerAddress <UserID> <New Port> <ConnectionID>`
- `AcknowledgePartnerAddress <UserID> <ConnectionID>`
- `Disconnect <UserID>`

With possible responses:

- `Redirect <New Address> <New Port>`
- `Okay`
- `OkayIntroduce <UserID>`
- `Wait`
- `Error`

Typing for the attributes:

- **UserID** is a unique string, used to identify the logical communication partner
- **ConnectionID** is a unique string, used to identify the current physical connection, to a logical communication partner
- **Port** is a string, containing the port number
- **Address** is a string, containing the IPv4 address or URL of a communication partner
- **Value** is a type of data in LDGV.  The VChans present in this Value are replaced with VChanSerials
- **Value Index** is an integer, containing the index of a Value
- **Type Name** is a  TName Type, of the desired Type
- **Type Structure** is a Type, containing the resolved Type of the Type Name

## Establishing a new Connection
As soon as B opens up their port with the accept command. A can connect, by sending an Introduce message to B.
This message contains the unique ID of A, As port, as well as the name and structure of the desired communication Type.
B then answers with a OkayIntroduce response, sharing their own unique ID with A.
Following that, A and B can send and recv values analog to Channels created with the new command.

## Sending messages over a Connection
When communication partner A executes a send command to send Value V to B, A first analyses V. 
Should V be or contain a Channel C, A will set a flag in C to redirect new messages to the address of B. 
After that, C will be converted to a serializable form, CS. 
With every Channel now being in a form, which can be sent over the network, A writes V to its write-buffer and sends B a NewValue message containing V. 
Upon receiving V as B with the recv command, B undoes the conversion of every Channel in V.
B then contacts the communication partner of each Channel, to inform them, that their new communication partner is now B, instead of A.
After this, B sends an acknowledgment (AcknowledgeValue message) back to A, which finalizes the sending of V.
A can now remove V out of its write-buffer.

## Responding to Messages
Except for the Introduce message, every message should be ideally answered with an Okay response. 
But in some cases the messages don't arrive at the right communication partner or at the wrong time. In these cases, other responses are used.
- **Redirect** responses are sent, when a message is sent to an outdated address
- **Wait** responses are sent, when a message cannot be handled at the current moment
    - This can be caused by currently being in a critical section while handling another message
    - During the sending process of a Channel
    - When the addressed Channel isn't yet known by the program
- **Error** responses are sent when an error occurs while handling a AcknowledgePartnerAddress message

## Informing Communication Partners of a Communication Partner Change
If there is a Channel C between A and B and A sends their side of the Channel to D, B needs to be made aware of that.
To archive this, D sends a NewPartnerAddress message to B. This message contains the server port of D and a new ConnectionID DC for D. 
B then replies with a AcknowledgePartnerAddress message, repeating DC. 
As soon as the address is established, C is considered successfully received by D.

## Shutting Down after Completing all Instructions
After A finishes the interpretation of their program, A waits until all messages they sent were acknowledged by their communication partners. After that, A sends a Disconnect message to all their peers. The Disconnect message is needed, since the recv command doesn't know the received Type during interpretation. Should the Disconnect message not exist, it would be theoretically possible to send a Unit-Type of an exhausted Channel to another communication partner. The recipient would now be unknowing whether their new communication partner is still online.

## Making Channels serializable
One of the main focuses of LDGVNW is to send Channels over the network.
VChans are Channels that are directly useable by LDGV, but since VChans can't be serialized directly, they need to be converted into VChanSerials first. VChans have the following (simplified) architecture:
`VChan <NetworkConnection> <Used>`

The contained NetworkConnection has this (simplified) architecture:

`NetworkConnection <ReadBuffer> <WriteBuffer> <PartnerID> <OwnID> <ConnectionState>`

The relevant part, for the conversion to VChanSerials, is found in the NetworkConnection.  The ReadBuffer contains Values, that are not yet handled, while the WriteBuffer contains Values, that are not yet acknowledged by the communication partner. The implementation of these Buffers is based on the implementation of the Chan type of Haskell, this is also noted and acknowledged in the Buffer module. The PartnerID and OwnID are strings to identify the logical communication partner, these do not change when sent to another communication partner. Lastly, the ConnectionState contains information about whether the connection is an external connection, an internal connection, offline or should be redirected to another communication partner.

The VChanSerial has the following architecture:

`VChanSerial <ReadList> <ReadOffset> <ReadLength> <WriteList> <WriteOffset> <WriteLength> <PartnerID> <OwnID> <Address> <Port> <ConnectionID>`

The ReadList contains the current elements of the ReadBuffer, the ReadOffset contains the logical index of the first element of the ReadBuffer, and the ReadLength is the number of all logical elements in the buffer. For example, let's say 5 Values were received, but the first 3 already were handled, so the ReadList would contain 2 elements, the ReadOffset would be 3 and the ReadLength would be 5. The WriteList, WriteOffset and WriteLength behave analogously. The PartnerID and OwnID are directly taken from the NetworkConnection and the Address, Port and ConnectionID (from the partner) are taken from the ConnectionState.

To convert a VChanSerial to a VChan, an empty VChan is simply filled with the data provided by the VChanSerial.

It is important to note that VChans only should be serialized after their ConnectionState has been set to Redirect. This freezes the VChan, as it can no longer receive new messages. This way it can be assured, that at the time of receipt, both the original VChan and the one generated from the VChanSerial contain identical data.

## Why Values are Acknowledged
LDGVNW has separate messages for sending a Value (NewValue) and acknowledging a Value (AcknowledgeValue). Simply knowing that the other party has received a Value, isn't enough when Channels are involved.
Let's say there is a Channel C, between A and B. A sends their side of C to D and at the same time, B sends their side of C to E. Since the sending of the Channel sides, happened simultaneously, D still thinks they are talking to B and E thinks they are talking to A. Should A and B now go offline, before either D or E, can contact them, to find out where they redirected their connections to, D and E will not be able to connect. Since acknowledgments are only sent after a sent Channel has been reconnected, it can be assured that D and E are connected, before A and B can go offline.

It would also be possible to use a Response to the NewValue message, to signal that the Value got acknowledged, but I decided to split this process into two messages, since the acknowledging can take long time, compared to other messages.

## Requesting a Value
Sending of most messages is only attempted once, this includes NewValue messages. A Value can be requested, by the recv command, using a RequestValue message.

## A communication example
The [communication example](README-networking-communication-example.md) gives a concrete demonstration of the communication protocol.

# Serializing and Sending Messages
The logical messages are serialized first, then are sent either using a fast protocol, which reuses existing connections or a stateless protocol, which was primary used during development, as a fallback when the fast protocol wasn't working yet. The fast protocol is enabled by default, switching protocols requires a small change to the networking code.

## Serialization
Messages and Responses in LDGVNW are serialized into ASCII-Strings and follow the form of the name of the Message, Value, etc. followed by their arguments in parentheses. For instance, the message `NewValue <abcd1234> <2> <VInt 42>` would be translated to `NNewValue (String:"abcd1234") (Int:2) (VInt (Int:42))`. The N in front of NewValue, signals that it belongs to the networking messages, the V in front of Int makes differentiating between Value Ints and other Ints easier.

To deserialize these messages, the alex and happy libraries are used.

## Stateless Protocol
The stateless protocol allows sending serialized logical messages directly. A new connection is established, followed by, sending the serialized message, waiting for a response and disconnecting afterward. Always creating new connections assures that every message gets its correct response, but establishing a new TCP connection every time a message is sent, also causes a performance penalty. The stateless protocol has a thread permanently looking for new messages. This thread creates a new temporary thread to handle each incoming message. Messages are primarily sent from the main thread, in which also the interpretation occurs, except for some messages, like the acknowledging of a new partner address, which is sent from the temporary thread.

## Fast Protocol
The fast protocol saves a once created TCP connection and reuses it as long as it stays open. Since LDGVNW uses multiple threads to send messages, this can lead to messages and responses being mismatched. To avoid this, each Message and Response is wrapped in a ConversationSession.

- `ConversationMessage <ConversationID> <Message>`
- `ConversationResponse <ConversationID> <Response>`
- `ConversationCloseAll`

The ConversationID is a random string, selected by the sender of the message and copied by the respondent. ConversationCloseAll is used when one party wants to close all connections to a peer, signaling to their peer that they would need to establish a new connection, if they would like to talk to this address and port again. 
This is helpful if there are A and B. A has an address and port combination of AP. After A and B are done communicating, A goes offline and sends a ConversationCloseAll. Now, C can reuse AP to talk to B.
The fast protocol also has a permanent thread, looking for new incoming connections. Each new TCP connection gets its own permanent thread, where new incoming messages and responses are collected. Each Channel also gets its own thread, where incoming messages get handled. Responses can be picked up by the sending function, to determine its further behavior.
Similar to the stateless protocol, most messages are sent from the main thread, while some messages are sent from a connection specific thread.

# Compatibility between Internal and External Channels
Internal Channels (Channels in the same program, typically created with new) and external Channels (Channels between two programs, typically created with connect and accept) are handled, for the most part, the same way in LDGVNW. Every Channel has a NetworkConnection. The NetworkConnection saves both incoming and outgoing messages and a ConnectionState. The ConnectionState dictates whether a NetworkConnection is internal or external. 
In contrast to external Channels, which serialize and send messages, internal Channels write the data of these messages directly to their counterparts.
Should an internal Channel be sent to a peer, the internal Channel gets converted into an external Channel. Should both sides of an external Channel end up in the same program, the connection will be converted to an internal Channel.




