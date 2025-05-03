/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "sdlstream.h"

/**
 * Close the input side of the specified stream.
 *
 * @param s Pointer to the Stream object to close.
 */
void
ws_close_input_stream(Stream s)
{
}

/**
 * Close the output side of the specified stream.
 *
 * @param s Pointer to the Stream object to close.
 */
void
ws_close_output_stream(Stream s)
{
}

/**
 * Fully close the specified stream.
 *
 * @param s Pointer to the Stream object to close.
 */
void
ws_close_stream(Stream s)
{
}

/**
 * Mark the specified stream as having input available.
 *
 * @param s Pointer to the Stream object.
 */
void
ws_input_stream(Stream s)
{
}

/**
 * Mark the specified stream as having no input available.
 *
 * @param s Pointer to the Stream object.
 */
void
ws_no_input_stream(Stream s)
{
}

/**
 * Begin listening on the specified socket for incoming connections or data.
 *
 * @param s Pointer to the Socket object to monitor.
 */
void
ws_listen_socket(Socket s)
{
}

/**
 * Write raw data to the specified stream.
 *
 * @param s Pointer to the Stream object.
 * @param data Pointer to the data buffer.
 * @param len Number of bytes to write.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_write_stream_data(Stream s, void *data, int len)
{
    return SUCCEED;
}

/**
 * Read raw data from the specified stream, with optional timeout.
 *
 * @param s Pointer to the Stream object.
 * @param data Pointer to the buffer to read into.
 * @param len Number of bytes to read.
 * @param timeout Timeout value in seconds.
 * @return Number of bytes read, or -1 on error.
 */
int
ws_read_stream_data(Stream s, void *data, int len, Real timeout)
{
    return 0;
}

/**
 * Clean up after a completed process.
 *
 * @param p Pointer to the Process object to finalize.
 */
void
ws_done_process(Process p)
{
}
