# metro

a simple tcp and udp socket server framework

## Quick start with example

```haskell
import Metro.Class
import Metro.Node
import Metro.TCP
import Metro.Servable
import Metro.Session (SessionT, makeResponse_)

data CustomPacket = CustomPacket { ... }
type CustomPacketId = ...

instance RecvPacket CustomPacket where
  recvPacket recv = ...
instance SendPacket CustomPacket where
  sendPacket pkt send = ...

instance GetPacketId CustomPacketId where
  getPacketId = ...
instance SetPacketId CustomPacketId where
  setPacketId k pkt = ...

type NodeId = ...
data CustomEnv = CustomEnv { ... }
type DeviceT = NodeT CustomEnv NodeId CustomPacketId CustomPacket
type DeviceEnv = NodeEnv1 CustomEnv NodeId CustomPacketId CustomPacket

sessionHandler = makeResponse_ $ \pkt -> ...

sessionGen :: IO CustomPacketId
sessionGen = ..

prepare :: Socket -> ConnEnv tp -> IO (Maybe (NodeId, CustomEnv))
prepare sock connEnv = Just ...

keepalive = 300

bind_port = "tcp://:8080"

startExampleServer = do
  sEnv <- initServerEnv "Example" (tcpConfig "tcp://:8080") sessionGen rawSocket prepare
  void $ forkIO $ startServer sEnv sessionHandler
```

more see [metro-example/src/Metro/Example.hs](metro-example/src/Metro/Example.hs)

## Projects use metro

- [haskell-hole](https://github.com/Lupino/haskell-hole) A hole to pass through the gateway. haskell version
- [metro-example](metro-example) A example use metro
