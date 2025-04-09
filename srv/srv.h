#ifndef SRV_H
#define SRV_H

#include <atomic>
#include <thread>

namespace srv {

template <typename Dispatcher> class AutoDispatch {
public:
  AutoDispatch(Dispatcher &&dispatcher, std::chrono::seconds timeout,
               std::chrono::milliseconds interval)
      : m_dispatcher(std::move(dispatcher)) {
    using namespace std::chrono;
    using namespace std::this_thread;

    const bool live_forever = timeout == 0s;
    const auto end = steady_clock::now() + timeout;
    m_thread = std::thread([=]() {
      while (!m_signal_stop && (live_forever || steady_clock::now() < end)) {
        m_dispatcher.dispatch();
        sleep_for(interval);
      }
    });
  }

  template <typename Timeout, typename Interval>
  AutoDispatch(Dispatcher &&dispatcher, Timeout timeout, Interval interval)
      : AutoDispatch(
            std::move(dispatcher),
            std::chrono::duration_cast<std::chrono::seconds>(timeout),
            std::chrono::duration_cast<std::chrono::milliseconds>(interval)) {}

  template <typename Interval>
  AutoDispatch(Dispatcher &&dispatcher, Interval timeout)
      : AutoDispatch(std::move(dispatcher), timeout,
                     std::chrono::milliseconds(50)) {}

  AutoDispatch(Dispatcher &&dispatcher)
      : AutoDispatch(std::move(dispatcher), std::chrono::seconds(0)) {}

  virtual ~AutoDispatch() noexcept { stop(); }

  AutoDispatch(AutoDispatch<Dispatcher> &&other) noexcept = default;

  AutoDispatch &operator=(AutoDispatch<Dispatcher> &&other) noexcept = default;

  const Dispatcher &operator*() const { return m_dispatcher; }

  Dispatcher &operator*() { return m_dispatcher; }

  const Dispatcher *operator->() const { return &m_dispatcher; }

  Dispatcher *operator->() { return &m_dispatcher; }

  void signal_stop() { m_signal_stop = true; }

  void stop() {
    signal_stop();
    join();
  }

  void join() {
    if (!m_thread.joinable()) {
      return;
    }

    m_thread.join();
  }

private:
  Dispatcher m_dispatcher;
  std::thread m_thread;
  std::atomic_bool m_signal_stop = false;
};

} // namespace srv

#endif
