
Elm.Native.Audio = {};
Elm.Native.Audio.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Audio = elm.Native.Audio || {};
    if (elm.Native.Audio.values) return elm.Native.Audio.values;
    
    // Imports
    var Signal = Elm.Native.Signal.make(elm);
    var Maybe = Elm.Maybe.make(elm);

    var TimeUpdate = {ctor : "TimeUpdate"};
    var Ended = {ctor : "Ended"};
    var Created = {ctor : "Created"};

    // Helper Functions... Do these exist already?
    function Tuple2(fst, snd){
        return {ctor: "_Tuple2", _0 : fst, _1 : snd};
    }

    function Properties(duration, currentTime, ended){
        return { _ : {}, duration : duration, currentTime : currentTime, ended : ended};
    }

    // Creates a Signal (Event, Properties)
    function audio(handler, path, alerts, propHandler, actions) {

        var sound = new Audio(path);
        var clock = new Object();
        var event = Signal.constant(Tuple2(Created, Properties(0,0,0)));

        var handle = handler({ "sound": sound, "clock": clock });

        function fireProp(eventCons){
            var props = Properties(sound.duration, sound.currentTime, sound.ended);
            elm.notify(event.id, Tuple2(eventCons, props));
            var action = propHandler(props);
            if(action.ctor == "Just")
                handle(action._0)
        }

        function addAudioListener(eventString, eventCons){
            sound.addEventListener(eventString, function () { fireProp(eventCons); });
        }

        if(alerts.timeupdate)
        {
            var timer;
            clock.start = function() { timer = setInterval(function(){ fireProp(TimeUpdate); }, 10); };
            clock.stop = function() { clearInterval(timer); };
        } else {
            clock.start = function() {};
            clock.stop = function() {};
        }

        if(alerts.ended)
            addAudioListener('ended', Ended);

        Signal.output('audio-handler',handle,actions);
        return event;
    }

    function play(o){
        o.sound.play();
        o.clock.start();
    }

    function pause(o){
        o.clock.stop();
        o.sound.pause()
    }

    function seek(o, time){
        o.sound.currentTime = time;
    }

    return elm.Native.Audio.values = {
        audio : F5(audio),
        play : play,
        pause : pause,
        seek : F2(seek)
    };

};

