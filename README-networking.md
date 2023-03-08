# How to try **LDGVNW** for yourself

Using **LDGVNW** is a little more difficult than simply starting a single program. In addition to the requirements of **LDGV** you also need a network connected via IPv4, should this network span more than one device the IP-addresses within the **LDGVNW** programs need to be altered in addition to that. The current version of **LDGVNW** was tested on Fedora 37 and MacOS 13.2 and should work on every recent Linux or MacOS machine, it is unknown whether **LDGVNW** works on Windows machines.

To run a **LDGVNW** example, found in the networking-examples folder, each program in the example folder need to be run at once. So if you would like to start the "handoff" example you would run following commands in different terminals or on different machines:

- stack run ldgv -- interpret networking-examples/handoff/client.ldgvnw
- stack run ldgv -- interpret networking-examples/handoff/server.ldgvnw
- stack run ldgv -- interpret networking-examples/handoff/handoff.ldgvnw

The order in which these commands are executed does not matter.

To test all the different test-cases in a easier way, they can be automatically run, with the scrips provided in the networking-tests folder. Simply running one of these scripts will run the whole test-case at once.
The testNW* scripts contain all the tests, except for the recursion test.


# An Introduction to **LDGVNW**s Networking Architecture

**LDGVNW** adds networking capabilities to **LDGV**. To enable this, **LDGVNW** adds 2 new commands:

- **accept \<Own Port> \<Own Type>**
- **connect \<Own Port> \<Own Type> \<Partner address> \<Partner Port>**

The **accept** command requires an integer as a port for others to connect, and a type that will be required of a connecting connection. Once a communication partner connects with a desired type, the **accept** command will return a **VChan**.
The **connect** command also requires an integer port and a desired type, but also needs to specify a string for the address of the connection partner as well as an integer for the port of the connection partner. Just like with the **accept** command, the **connect** command will return a **VChan** once a connection has been established. 
Important to note is that, with the current implementation, only IPv4 addresses are supported. IPv6 and Unix domain sockets could be supported in the future with a relatively low effort.

## The Logical Communication Architecture

### Messages and Responses
In **LDGVNW** there are 7 possible **Messages** and 4 possible **Responses**.
The messages are:

- **Introduce \<UserID> \<Own Port> \<Type Name> \<Type Structure>**
- **NewValue \<UserID> \<Value Index> \<Value>**
- **RequestValue \<UserID> \<Value Index>**
- **AcknowledgeValue \<UserID> \<Value Index>**
- **NewPartnerAddress \<UserID> \<New Port> \<ConnectionID>**
- **AcknowledgePartnerAddress \<UserID> \<ConnectionID>**
- **Disconnect \<UserID>**

With possible responses:

- **Redirect \<New Address> \<New Port>**
- **Okay**
- **OkayIntroduce \<UserID>**
- **Wait**

Typing for the attributes:

- **UserID** A unique string, used to identify the logical communication partner
- **ConnectionID** A unique string, used to identify the physical communication partner
- **Port** A string containing the number of a port
- **Address** A string containing the IPv4 or URL of a communication partner
- **Value** The serialization of the sent Value
- **Value Index** A integer containing the index of a Value
- **Type Name** The serialization of the TName Type of the desired Type
- **Type Structure** The serialization of the type structure of the desired Type

The **UserID** is a unique identifier, so a message can be associated with the correct communication partner.

### Establishing a new Connection
As soon as **B** opens up their port with the **accept** command. **A** can **connect**, by sending a **Introduce** message to **B**. This message contains the unique ID of **A**, **A**s port as well as the name and structure of the desired communication **Type**. **B** then answers with a **OkayIntroduce** response, sharing their own unique ID with **A**. Following that **A** and **B** can **send** and **recv** values analog to **Channels** created with the **new** command.

### Sending messages over a Connection
When communication partner **A** executes a send instruction to **send** Value **V** to **B**, **A** first analyses **V**. Should **V** be or contain a **Channel** **C**, receiving messages for **C** will be redirected to the address of **B** and the state of **C** will be converted to a serializable form **CS**. After that **V** will be serialized to **VS**, which will be written to **A**s write-buffer and sent to **B** via a **NewValue** message. Upon receiving **VS** as **B** with the **recv** instruction, **VS** will be deserialized to **VD**. Should **VD** contain a serialized form of a **Channel** this **Channel** will be converted to a regular **Channel** **CD** and the communication partner of **CD** will be informed of the communication partner change. After this **B** sends an acknowledgment (**AcknowledgeValue** message) back to **A**, which finalizes the sending of **V**, by **A** removing **V** out of its write-buffer.

### Responding to Messages
With the exception of the **Introduce** message, every message should be answered with a **Okay** response. Exceptions to that are **Redirect** responses, which are used when a message is sent to an outdated address or **Wait** responses which are sent when a message cannot be handled at the current moment. This can happen when the communication partner is already handling a message in a critical section, or a communication partner is currently in the progress of sending the **Channel** which the message is sent to.

### Informing communication partners of a communication partner change
If a **Channel** **C** got sent over a network connection to **A**, the new communication partner **B** needs to be notified of this change. To archive this **A** sends a **NewPartnerAddress** message to **B**. This message contains the server port of **A** and the new ConnectionID **AC** for **A**. **B** then replies with a **AcknowledgePartnerAddress** message, repeating **AC**. As soon as the address is established **C** is considered being successfully received by **A**.


### Shutting down after completing all the instructions
After **A** finishes the interpretation of their program, **A** waits until all messages it sent were acknowledged by their communication partners. After that **A** sends a **Disconnect** message to all its peers. The **Disconnect** message is needed to avoid rewriting a large portion of the interpreter to annotate each expression with their associated output **Types**. Should the **Disconnect** message not exist, it would be theoretically possible to send a **Unit=Type** of an exhausted **Channel** to another communication partner. The recipient would now be unknowing whether their new communication partner were still online.

### A communication example

In the README-networking-communication-example.md is an example explaining the communication protocol on a concrete example

## Serializing and Sending Messages
The logical messages are serialized first, then are sent either using a fast protocol which reuses existing connections or a stateless protocol, which was primary used during development as a fallback when the fest protocol wasn't working, yet.

### Serialization
Messages and Responses in **LDGVNW** are serialized into ASCII-Strings and follow usually the form of the name of the Message, Value, etc. followed by their arguments in brackets. For instance the message **NewValue \<abcd1234> \<2> \<VInt 42>** would be translated to **NNewValue (String:"abcd1234") (Int:2) (VInt (Int:42))**

### Stateless Protocol
The stateless protocol allows to directly send serialized logical messages, by establishing a new connection, sending the serialized message, waiting for a response and disconnecting afterward. By always creating new connections it can be assured that every Message gets their correct response, but establishing a new TCP connection every time a message is sent also causes a huge performance penalty.

### Fast Protocol
The fast protocol saves a once created TCP connection and reuses it as long as it stays open. Since **LDGVNW** uses multiple threads to send Messages this can lead to Messages and Responses to be mismatched. To avoid this each Message and Response is wrapped in a ConversationSession.

- **ConversationMessage \<ConversationID> \<Message>**
- **ConversationResponse \<ConversationID> \<Response>**
- **ConversationCloseAll**

The ConversationID is a random string, selected by the sender of the message and copied by the respondent. **ConversationCloseAll** is used when one party wants to close all connections to a peer, signaling to their peer that they would need to establish a new connection if they would like to talk to this port again.

## Compatibility between Internal and External Channels

Internal channels (channels in the same program, created with **new**) and external channels (channels between two programs, created with **connect** and **accept**) are handled for the most part the same way in **LDGVNW**. Every channel has a **NetworkConnection** object, which saves both incoming and outgoing messages, it also has a **ConnectionState** object, which dictates whether a **NetworkConnection** is internal or external. Should a internal connection be send to a peer, both the internal connection gets converted into an external connection. Should both sides of a external connection end up in the same program, the connection will be converted to an internal connection.
