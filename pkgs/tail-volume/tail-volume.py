import asyncio
import signal
import traceback
from contextlib import suppress

import pulsectl_asyncio
from pulsectl import PulseIndexError


async def get_default_sink_name(pulse: pulsectl_asyncio.PulseAsync):
    server_info = await pulse.server_info()
    return server_info.default_sink_name


def format_volume(sink):
    return round(sink.volume.value_flat * 100)


async def listen_events():
    async with pulsectl_asyncio.PulseAsync('event-printer') as pulse:
        default_sink_name = await get_default_sink_name(pulse)
        try:
            sink = await pulse.get_sink_by_name(default_sink_name)
            print(format_volume(sink), flush=True)
        except PulseIndexError:
            traceback.print_exc()
        async for event in pulse.subscribe_events('sink', 'server'):
            default_sink_name = await get_default_sink_name(pulse)
            try:
                sink = await pulse.get_sink_by_name(default_sink_name)
                print(format_volume(sink), flush=True)
            except PulseIndexError:
                traceback.print_exc()


async def main():
    listen_task = loop.create_task(listen_events())

    # register signal handlers to cancel listener when program is asked
    # to terminate
    # Alternatively, the PulseAudio event subscription can be ended by
    # breaking/returning from the `async for` loop
    for sig in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT):
        loop.add_signal_handler(sig, listen_task.cancel)

    with suppress(asyncio.CancelledError):
        await listen_task


if __name__ == '__main__':
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    loop.run_until_complete(main())
