#include "Receiver.h"

#include <gpiod.h>
#include <linux/gpio.h>
#include <sys/ioctl.h>
#include <cmath>
#include <cstdio>

Receiver::Receiver(const int& pin)
  :mPin(pin),
   mTimeout(-1),
   mStopReceiverThread(false),
   mReceiverThreadIsAlive(false)
{
  mStartCondition = PTHREAD_COND_INITIALIZER;
  mStartMutex = PTHREAD_MUTEX_INITIALIZER;
}

Receiver::~Receiver()
{
  stop();
  pthread_mutex_destroy(&mStartMutex);
  pthread_cond_destroy(&mStartCondition);
  mPin = -1;
}

bool Receiver::start()
{
  if((0 < mPin) && (mPin < 41) && !mReceiverThreadIsAlive) {
    mStopReceiverThread = false;
    if(pthread_create(&mReceiverThread, nullptr, receive, this) == 0) {
      pthread_mutex_lock(&mStartMutex);
      while(!mReceiverThreadIsAlive) {
        if(pthread_cond_wait(&mStartCondition, &mStartMutex) != 0) {
          mReceiverThreadIsAlive = false;
          break;
        }
      }
      pthread_mutex_unlock(&mStartMutex);
    }
  }
  return mReceiverThreadIsAlive;
}

bool Receiver::stop()
{
  if(mReceiverThreadIsAlive) {
    mStopReceiverThread = true;
    pthread_join(mReceiverThread, nullptr);
  }
  return !mReceiverThreadIsAlive;
}

void Receiver::setTimeout(const int& timeout)
{
  if(!mReceiverThreadIsAlive) {
    mTimeout = timeout;
  }
}

bool Receiver::getNextPulse(PulseDurationType& duration) const
{
  return mPulseData.try_dequeue(duration);
}

void* Receiver::receive(void* parameter)
{
  struct gpiod_line_request* line = nullptr;
  Receiver* receiver = reinterpret_cast<Receiver*>(parameter);

  static char buffer[FILENAME_MAX] = { '\0' };
  unsigned int pin = static_cast<unsigned int>(receiver->mPin.load());
  pthread_mutex_lock(&receiver->mStartMutex);

  static const char device[] = "/dev/gpiochip0";
  struct gpiod_chip* chip = gpiod_chip_open(&device[0]);
  if(chip != nullptr) {
    struct gpiod_line_settings* settings = gpiod_line_settings_new();
    if(settings != nullptr) {
      struct gpiod_line_config* lConfiguration = nullptr;
      if(gpiod_line_settings_set_direction(settings, GPIOD_LINE_DIRECTION_INPUT) == 0) {
        if(gpiod_line_settings_set_edge_detection(settings, GPIOD_LINE_EDGE_BOTH) == 0) {
          if(gpiod_line_settings_set_event_clock(settings, GPIOD_LINE_CLOCK_REALTIME) == 0) {
            lConfiguration = gpiod_line_config_new();
            if(lConfiguration != nullptr) {
              if(gpiod_line_config_add_line_settings(lConfiguration, &pin, 1, settings) != 0) {
                gpiod_line_config_free(lConfiguration);
                lConfiguration = nullptr;
                snprintf(buffer, sizeof(buffer), "Unable to set line configuration for pin %d", pin);
                perror(buffer);
              }
            } else {
              snprintf(buffer, sizeof(buffer), "Unable to create line configuration for pin %d", pin);
              perror(buffer);
            }
          } else {
            snprintf(buffer, sizeof(buffer), "Unable to set line clock for pin %d", pin);
            perror(buffer);
          }
        } else {
          snprintf(buffer, sizeof(buffer), "Unable to set line edge detection for pin %d", pin);
          perror(buffer);
        }
      } else {
        snprintf(buffer, sizeof(buffer), "Unable to set line direction for pin %d", pin);
        perror(buffer);
      }

      if(lConfiguration != nullptr) {
        struct gpiod_request_config* rConfiguration = gpiod_request_config_new();
        if(rConfiguration != nullptr) {
          gpiod_request_config_set_consumer(rConfiguration, "hideki");
          line = gpiod_chip_request_lines(chip, rConfiguration, lConfiguration);
          gpiod_request_config_free(rConfiguration);
          rConfiguration = nullptr;
        } else {
          snprintf(buffer, sizeof(buffer), "Unable to create request configuration for pin %d", pin);
          perror(buffer);
        }
        gpiod_line_config_free(lConfiguration);
        lConfiguration = nullptr;
      } else {
        snprintf(buffer, sizeof(buffer), "Unable to create line configuration for pin %d", pin);
        perror(buffer);
      }
      gpiod_line_settings_free(settings);
      settings = nullptr;
    } else {
      snprintf(buffer, sizeof(buffer), "Unable to create settings for pin %d", pin);
      perror(buffer);
    }
    gpiod_chip_close(chip);
    chip = nullptr;
  } else {
    snprintf(buffer, sizeof(buffer), "Unable to open gpio device for pin %d", pin);
    perror(buffer);
  }

  pthread_cond_signal(&receiver->mStartCondition);
  pthread_mutex_unlock(&receiver->mStartMutex);

  // Start receiver
  if(line != nullptr) {
    struct gpiod_edge_event_buffer* events = gpiod_edge_event_buffer_new(1);
  	if(events != nullptr) {
      receiver->mReceiverThreadIsAlive = true;
      while(!receiver->mStopReceiverThread) {
        static const int64_t timeout = static_cast<int64_t>(receiver->mTimeout.load());
        switch(gpiod_line_request_wait_edge_events(line, 1000000 * timeout)) {
          case 0: { // Time out --> skip
            snprintf(buffer, sizeof(buffer), "Timed out waiting edge change on pin %d", pin);
            perror(buffer);
            break;
          }
          case 1: { // Pending event --> read
            if(gpiod_line_request_read_edge_events(line, events, 1) > 0) {
              struct gpiod_edge_event* event = gpiod_edge_event_buffer_get_event(events, 0);
              if(event != nullptr) {
                static uint64_t tNew = 0;
                static uint64_t tOld = 0;
                tNew = gpiod_edge_event_get_timestamp_ns(event);

                const uint64_t duration = tNew - tOld;
                if(duration < static_cast<decltype(duration)>(PulseDurationType::max().count())) {
                  receiver->mPulseData.enqueue(PulseDurationType(duration));
                } else {
                  snprintf(buffer, sizeof(buffer), "Got invalid pulse length %lu for pin %d", duration, pin);
                  perror(buffer);
                }

                tOld = tNew;
              }
            } else {
              snprintf(buffer, sizeof(buffer), "Unable to get edge change events on pin %d", pin);
              perror(buffer);
            }
            break;
          }
          default: { // Error --> skip
            snprintf(buffer, sizeof(buffer), "Error during waiting edge change on pin %d", pin);
            perror(buffer);
            break;
          }
        }
      }
      receiver->mReceiverThreadIsAlive = false;
      gpiod_edge_event_buffer_free(events);
      events = nullptr;
  	} else {
      snprintf(buffer, sizeof(buffer), "Unable to create event buffer for pin %d", pin);
      perror(buffer);
  	}
    gpiod_line_request_release(line);
    line = nullptr;
  } else {
    snprintf(buffer, sizeof(buffer), "Unable to request line for pin %d", pin);
    perror(buffer);
  }

  return nullptr;
}
