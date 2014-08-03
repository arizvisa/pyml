import ctypes as ct,logging,sys
## definitions
lib = ct.WinDLL('ml32i3.dll')
errstr = {0: 'Everything OK', 1: 'Link Dead', 2: 'Link read inconsistant data', 3: 'Get out of sequence', 4: 'PutNext passed bad token', 5: 'Put out of sequence', 6: 'PutData given too much data', 7: 'Machine number overflow', 8: 'Out of memory', 9:'Failure to accept socket connection', 10: 'Deferred connection still unconnected', 11: 'The other side of the connection closed the link, you may yet get undelivered data', 12: 'Internal mathlink library error', 13: 'Link cannot be duplicated', 15: 'No acknowlodgement?', 16: 'No Data?', 17: 'Packet Not delivered?', 18: 'No Message?', 19: 'Failed?', 20: 'Unknown', 21: 'Unexpected call of PutEndPacket', 22: 'NextPacket called while current packet has unread data', 23: 'NextPacket read in an unknown packet head', 24: 'Unexpected end of packet', 25: 'A put or get was aborted before affecting the link', 26: 'Internal mathlink library error', 27: 'Unknown', 28: 'Unknown', 29: 'Unknown', 30: 'Feature not currently implemented', 32: 'Mathlink environment not initialized', 33: 'Insufficient arguments to open link', 34: 'Protocol unavailable', 35: 'Mode unavailable', 36: 'Launch unsupported', 37: 'Cannot launch the program again from the same file', 38:'Insufficient space to launch the program', 39: 'Found no parent to connect to',40: 'Link name already in use', 41: 'Link name not found to be listening', 42:'Link name missing or not in proper form', 43: 'Location unreachable or not in proper form', 44: 'A required resource is unavaible', 45: 'Program failed to launch due to a missing resource or library', 46: 'Launch failed because of inability to find program', 47: 'character data in given encoding incorrect', 48: 'unable to convert from given character encoding to link encoding', 49: 'unable to convert from link encoding to requested encoding', 996: 'Unknown mathlink internal', 997: 'Unknown mathlink internal', 998: 'Unknown mathlink internal', 999: 'Failure of an internal assertion', 1000: 'Start of user defined errors', -1: 'Unknown mathlink error message'} 
argv = []
argv.append('-linkconnect hello')

## connect with mathlink
_argv_t = ct.c_char_p * (len(argv)+1)
_argv = _argv_t(*(ct.c_char_p(x) for x in argv))
_argv[len(argv)] = 0

mlenv = lib.MLInitialize(0)
errno = ct.c_int()
lp = lib.MLOpenString(mlenv, ct.c_char_p("-linkconnect -linkname hello"), ct.byref(errno))
#lp = lib.MLOpenArgcArgv(mlenv, len(_argv), ct.byref(_argv), ct.byref(errno))
print 'initialized environment : error %d(%s)'% (errno.value, errstr[errno.value])

lib.MLLinkName.restype = ct.c_char_p
_ = lib.MLLinkName(lp)
print 'connected to link program %s'% _

_ = lib.MLActivate(lp)
if _ == 0:
    _ = lib.MLError(lp)
    print 'MLActivate -> error %d : %s'%(_, errstr[_])
    sys.exit(0)

## rpl
print 'entering rpl'
index = 0
try:
    while True:
        # check if any oob messages are available
        _ = lib.MLMessageReady(lp)
        if _ != 0:
            code,param = ct.c_int(),ct.c_int()
            _ = lib.MLGetMessage(lp, ct.byref(code), ct.byref(param))
            print '[%d] MLGetMessage -> result : %x -> 0x%x(%d) -> 0x%x(%d)'%(index, _, code.value,param.value)

        # check if anything is available
        _ = lib.MLFlush(lp)
        if _ == 0:
            _ = lib.MLError(lp)
            print '[%d] MLFlush -> error %d : %s'%(index,_,errstr[_])
            break
        
        _ = lib.MLReady(lp)
        if _ == 0:
            continue

        # read/print
        _ = lib.MLGetNext(lp)
#        if _ != 0:
#            print '[%d] MLGetNext -> error %d : %s'%( index, _, errstr[_])
        print '[%d] MLGetNext -> %d'%(index,_)

        type = lib.MLGetType(lp)
        count = ct.c_int()
        _ = lib.MLBytesToGet(lp, ct.byref(count))
        if _ == 0:
            _ = lib.MLError(lp)
            print '[%d] MLBytesToGet -> error %d : %s'%( index, _, errstr[_])
            break

        count = count.value
        print '[%d] MLBytesToGet -> type : %d -> count : %x'%(index,type,count)
        
        buf = ct.c_buffer(count)
        r_count = ct.c_int()
        _ = lib.MLGetData(lp, ct.byref(buf), count, ct.byref(r_count))
        if _ == 0:
            _ = lib.MLError(lp)
            print '[%d] MLGetData[%x] -> error %d : %s'%( index, count, _, errstr[_])
        else:
            print '[%d] MLGetData[%x] -> %s'%(index, r_count.value, repr(buf.raw))
        index += 1

except KeyboardInterrupt:
    pass

## tear down the mathlink connection
_ = lib.MLClose(lp)
print 'closed -> %d'% _
_ = lib.MLDeinitialize(mlenv)
print 'deinitialized -> %d'% _
print 'done'
