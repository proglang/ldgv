# A Communication Example of Networking in **LDGVNW**

The following log shows an conversation of 4 conversation partners, running the "bidirhandoff" test.

```
Explanation: Client connects for the first time to the server
Client(4343)->Server(4242)
Message: NIntroduce (String:"qV8Xo421") (String:"4343") (TName (Bool:False) (String:"SendInt")) (TSend (String:"#!") (TInt) (TRecv (String:"#?") (TInt) (TSend (String:"#!") (TInt) (TRecv (String:"#?") (TInt) (TUnit)))))
Response: NOkayIntroduce (String:"Y1lPJ1sM")

Explanation: Client sends the first value to the server
Client(4343)->Server(4242)
Message: NNewValue (String:"qV8Xo421") (Int:0) (VInt (Int:1)) 
Response: NOkay

Explanation: Server acknowledges the first value from the client
Server(4242)->Client(4343)
Message: NAcknowledgeValue (String:"Y1lPJ1sM") (Int:0)
Response: NOkay

Explanation: Server sends the first value to the client
Server(4242)->Client(4343)
Message: NNewValue (String:"Y1lPJ1sM") (Int:0) (VInt (Int:1300)) 
Response: NOkay

Explanation: Client acknowledges the first value from the server
Client(4343)->Server(4242)
Message: NAcknowledgeValue (String:"qV8Xo421") (Int:0)
Response: NOkay

Explanation: ServerHandoff connects to the Server
ServerHandoff(4240)->Server(4242)
Message: NIntroduce (String:"nCzR17XT") (String:"4240") (TName (Bool:True) (String:"SendSendIntServer")) (TSend (String:"#!") (TName (Bool:False) (String:"SendIntServer")) (TUnit)
Response: NOkayIntroduce(String:"unFbpEeg")

Explanation: Client connects to the ClientHandoff
Client(4343)->ClientHandoff(4340)
Message: NIntroduce (String:"54AVQX89") (String:"4343") (TName (Bool:False) (String:"SendSendIntClient")) (TSend (String:"#!") (TName (Bool:False) (String:"SendIntClient")) (TUnit))
Response: NOkayIntroduce (String:"dDF0Te3V")

Explanation: Server sends the channel to the ServerHandoff, there are no values in the Handoff since all values are already acknowledged
Server(4242)->ServerHandoff(4240)
Message: NNewValue (String:"unFbpEeg") (Int:0) (VChanSerial (((SValuesArray []) (Int:1) (Int:1))) (((SValuesArray []) (Int:1) (Int:1))) (String:"qV8Xo421") (String:"Y1lPJ1sM") (((String:"127.0.0.1") (String:"4343") (String:"qV8Xo421"))))
Response: NOkay

Explanation: Client sends the channel to the ClientHandoff
Client(4343)->ClientHandoff(4340)
Message: NNewValue (String:"54AVQX89") (Int:0) (VChanSerial (((SValuesArray []) (Int:1) (Int:1))) (((SValuesArray []) (Int:1) (Int:1))) (String:"Y1lPJ1sM") (String:"qV8Xo421") (((String:"127.0.0.1") (String:"4242") (String:"Y1lPJ1sM")))) 
Response: NOkay

Explanation: ClientHandoff wants to introduce itself to the Server, but fails since the channel is now owned by the ServerHandoff
ClientHandoff(4340)->Server(4242)
Message: NNewPartnerAddress (String:"qV8Xo421") (String:"4340") (String:"hh0kAZdY")
Response: NRedirect (String:"127.0.0.1") (String:"4240")

Explanation: ServerHandoff wants to introduce itself to the Client, but fails since the channel is now owned by the ClientHandoff
ServerHandoff(4240)->Client(4343)
Message: NNewPartnerAddress (String:"Y1lPJ1sM") (String:"4240") (String:"OUN8jvH1") 
Response: NRedirect (String:"127.0.0.1") (String:"4240")

Explanation: ClientHandoff introduces itself to the ServerHandoff
ClientHandoff(4340)->ServerHandoff(4240)
Message: NNewPartnerAddress (String:"qV8Xo421") (String:"4340") (String:"hh0kAZdY")
Response: NOkay

Explanation: ServerHandoff introduces itself to the ClientHandoff
ServerHandoff(4240)->ClientHandoff(4340)
Message: NNewPartnerAddress (String:"Y1lPJ1sM") (String:"4240") (String:"OUN8jvH1") 
Response: NOkay

Explanation: ClientHandoff acknowledges the new address of the ServerHandoff
ClientHandoff(4340)->ServerHandoff(4240)
Message: NAcknowledgePartnerAddress (String:"qV8Xo421") (String:"OUN8jvH1")
Response: NOkay

Explanation: ServerHandoff acknowledges the new address of the ClientHandoff
ServerHandoff(4240)->ClientHandoff(4340)
Message: NAcknowledgePartnerAddress (String:"Y1lPJ1sM") (String:"hh0kAZdY")
Response: NOkay

Explanation: Since the ServerHandoff has acknowledged the new address, the ClientHandoff can now acknowledge the successful receiving of the Channel to the Client
ClientHandoff(4340)->Client(4343)
Message: NAcknowledgeValue (String:"dDF0Te3V") (Int:0)
Response: NOkay

Explanation: Since the ClientHandoff has acknowledged the new address, the ServerHandoff can now acknowledge the successful receiving of the Channel to the Server
ServerHandoff(4240)->Server(4242)
Message: NAcknowledgeValue (String:"nCzR17XT") (Int:0)
Response: NOkay


ClientHandoff(4340)->ServerHandoff(4240)
Message: NNewValue (String:"qV8Xo421") (Int:1) (VInt (Int:41))
Response: NOkay

Explanation: Since the client now has all of the values of its active connections acknowledged, it can disconnect
Client(4343)->ClientHandoff(4340)
Message: NDisconnect (String:"54AVQX89")
Response: NOkay

Explanation: Since the server has now all of the values of its active connections acknowledged, it can disconnect
Server(4242)->ServerHandoff(4240)
Message: NDisconnect (String:"unFbpEeg")
Response: NOkay

ServerHandoff(4240)->ClientHandoff(4340)
Message: NAcknowledgeValue (String:"Y1lPJ1sM") (Int:1)
Response: NOkay

ServerHandoff(4240)->ClientHandoff(4340)
Message: NNewValue (String:"Y1lPJ1sM") (Int:1) (VInt (Int:37))
Response: NOkay

ClientHandoff(4340)->ServerHandoff(4240)
Message: NAcknowledgeValue (String:"qV8Xo421") (Int:1)
Response: NOkay

ServerHandoff(4240)->ClientHandoff(4340)
Message: NDisconnect (String:"Y1lPJ1sM")
Response: NOkay
```