TARGET=libhideki.so

CC         = gcc
INCLUDE	   = -I./queue \
             -I/$(JAVA_HOME)/include -I/$(JAVA_HOME)/include/linux  \
             -I/usr/lib/jvm/temurin-21-jdk-arm64/include       \
             -I/usr/lib/jvm/temurin-21-jdk-arm64/include/linux \
             -I/usr/local/include -I/usr/local/include/linux

CFLAGS      := -O3 -std=c++20 -fPIC -pedantic -Wall -Winline -pipe -flto $(CARGS) 
LIBS         = -lgpiod -lm -lpthread -lstdc++

CLASS_PATH = ../target
vpath %.class $(CLASS_PATH)

# Should not alter anything below this line
###############################################################################
SRC	=   CC1101.cpp Decoder.cpp Receiver.cpp RXB.cpp HidekiJniWrapper.cpp

OBJ =	$(SRC:.c=.o)

all:	$(OBJ)
	@$(CC) $(OBJ) $(CFLAGS) -shared -o $(TARGET) $(INCLUDE) $(LIBS)

.cpp.o:
	@echo [COMPILE] $<
	@$(CC) -c $(CFLAGS) $(INCLUDE) $< -o $@

clean:
	rm -f $(OBJ) $(TARGET) *~ core tags Makefile.bak

tags:	$(SRC)
	@echo [ctags]
	@ctags $(SRC)

depend:
	makedepend -Y $(SRC)

install:	$(TARGET)
	@echo [install]
	install -m 0755 -d /usr/local/lib
	install -m 0755 -d /usr/local/include
	install -m 0644 $(TARGET) /usr/local/lib

uninstall:
	@echo [uninstall]
	rm -f /usr/local/lib/$(TARGET)

CC1101.o: CC1101.h
Decoder.o: Decoder.h
Receiver.o: Receiver.h
RXB.o: RXB.h
HidekiJniWrapper.o: HidekiJniWrapper.h

#HidekiDecoder.h : HidekiDecoder.class
#	javah -classpath $(CLASS_PATH) $*

