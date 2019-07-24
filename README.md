# metro

a simple tcp socket server framework

## Quick start with example

```
import Metro.Class
import Metro.Node
import Metro.Session (SessionT, makeResponse_)

data CustomPacket = CustomPacket { ... }
type CustomPacketId = ...

instance Packet CustomPacket where
  recvPacket recv = ...
  sendPacket pkt send = ...

instance PacketId CustomPacketId PacketId
  getPacketId = ...
  setPacketId k pkt = ...

type NodeId = ...
data CustomEnv = CustomEnv { ... }
type DeviceT = NodeT CustomEnv NodeId CustomPacketId CustomPacket
type DeviceEnv = NodeEnv1 CustomEnv NodeId CustomPacketId CustomPacket

sessionHandler = makeResponse_ $ \pkt -> ...

sessionGen :: IO CustomPacketId
sessionGen = ..

getDeviceId :: CustomPacket -> NodeId
getDeviceId pkt = ..

keepalive = 300

bind_port = "tcp://:8080"

startExampleServer = do
  sock <- listen bind_port
  sEnv <- initServerEnv sock (fromIntegral keepalive) (CustomEnv {..}) sessionGen getDeviceId
  void $ forkIO $ startServer sEnv rawSocket sessionHandler
```
