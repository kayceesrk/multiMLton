signature CHANNEL =
sig
    type 'a chan

    val channel: unit -> 'a chan
    val sameChannel: 'a chan * 'a chan -> bool

    val aSend : ('a chan * 'a) -> unit
    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a

    val sendEvt  : ('a chan * 'a) -> unit Event.sevt
    val recvEvt  : 'a chan -> 'a Event.sevt

    val sendPoll : ('a chan * 'a) -> bool
    val recvPoll : 'a chan -> 'a option

    val aSendEvt : ('a chan * 'a) -> (unit, unit) Event.aevt
    val aRecvEvt  : 'a chan -> (unit, 'a) Event.aevt
end

signature CHANNEL_EXTRA =
sig
  include CHANNEL

end
