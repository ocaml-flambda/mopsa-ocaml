open Manager

type 'a with_channel = {
  value : 'a;
  channels : Post.channel list;
}

let return ?(channels = []) a = {
  value = a;
  channels;
}

let bind (f: 'a -> 'b with_channel) (a: 'a with_channel) : 'b with_channel =
  let b = f a.value in
  {b with channels = a.channels @ b.channels}

let map_env (tk:token) (f:'t -> 't with_channel) (man:('a, 't) man) (flow:'a flow) : 'a flow * Post.channel list=
  let a = f (Flow.get_env tk man flow) in
  Flow.set_env tk a.value man flow, a.channels
