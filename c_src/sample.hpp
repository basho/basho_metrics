// -------------------------------------------------------------------
//
// basho_metrics: fast performance metrics for Erlang.
// 
// inspired and partially derived from Coda Hale's 'metrics' 
// Copyright (c) 2010-2001 Coda Hale
// https://github.com/codahale/metrics/blob/development/LICENSE.md
//
// Copyright (c) 2011 Basho Technologies, Inc. All Rights Reserved.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
#ifndef SAMPLE_HPP_
#define SAMPLE_HPP_

#include <map>
#include <vector>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include <stdint.h>
#include <sys/time.h>
#include <boost/circular_buffer.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>


/**
 * An exponentially-decaying random sample of {@code long}s. Uses Cormode et
 * al's forward-decaying priority reservoir sampling method to produce a
 * statistically representative sample, exponentially biased towards newer
 * entries.
 *
 * @see <a href="http://www.research.att.com/people/Cormode_Graham/library/publications/CormodeShkapenyukSrivastavaXu09.pdf">
 * Cormode et al. Forward Decay: A Practical Time Decay Model for Streaming
 * Systems. ICDE '09: Proceedings of the 2009 IEEE International Conference on
 * Data Engineering (2009)</a>
 */
template <typename IntType=unsigned long>
struct exponentially_decaying_sample
{
    exponentially_decaying_sample(std::size_t size, double alpha=0.015)
        : size_(size),
          alpha_(alpha),
          count_(0),
          start_time_(tick()),
          next_scale_time_(start_time_ + RESCALE_THRESHOLD)
    {
    }
        
    /**
     * Clears all recorded values.
     */
    void clear()
    {
        values_.clear();
        count_ = 0;
        start_time_ = tick();
        next_scale_time_ = start_time_ + RESCALE_THRESHOLD;
    }

   /**
     * Returns the number of values recorded.
     */
    std::size_t size() const
    {
        return std::min(size_, count_);
    }

    double weight(long t) const
    {
        return std::exp(alpha_ * t);
    }

    /**
     * Adds a new recorded value to the sample.
     */    
    void update(IntType value) 
    {
        update(value, tick());
    }

    void update(IntType value, long timestamp)
    {
        double priority = weight(timestamp - start_time_) / next_random();
        if (++count_ <= size_)
            values_[priority] = value;
        else
        {
            double first = values_.begin()->first;
            if (first < priority)
            {
                if (values_.find(priority) == values_.end() )
                    values_[priority] = value;
                values_.erase(values_.begin());
            }
        }
        long now = tick();
        if (now > next_scale_time_)
            rescale(now, next_scale_time_);
    }

    /* "A common feature of the above techniques—indeed, the key technique 
     * that allows us to track the decayed weights efficiently—is that they 
     * maintain counts and other quantities based on g(ti − L), and only scale 
     * by g(t − L) at query time. But while g(ti −L)/g(t−L) is guaranteed to 
     * lie between zero and one, the intermediate values of g(ti − L) could 
     * become very large. For polynomial functions, these values should not 
     * grow too large, and should be effectively represented in practice by 
     * floating point values without loss of precision. For exponential 
     * functions, these values could grow quite large as new values of (ti − L)
     * become large, and potentially exceed the capacity of common floating 
     * point types. However, since the values stored by the algorithms are 
     * linear combinations of g values (scaled sums), they can be rescaled 
     * relative to a new landmark. That is, by the analysis of exponential 
     * decay in Section III-A, the choice of L does not affect the final 
     * result. We can therefore multiply each value based on L by a factor of 
     * exp(−α(L′ − L)), and obtain the correct value as if we had instead 
     * computed relative to a new landmark L′ (and then use this new L′ at 
     * query time). This can be done with a linear pass over whatever data 
     * structure is being used."
     */
    void rescale(long now, long next) 
    {
        next_scale_time_ = now + RESCALE_THRESHOLD;
        long old_start_time = start_time_;
        start_time_ = tick();
        std::map<double, IntType> new_values;
        for (typename std::map<double, IntType>::const_iterator 
             it=values_.begin();
             it != values_.end();
             ++it)
        {
            IntType value = it->second;
            new_values[it->first * std::exp(-alpha_ * (start_time_-old_start_time))] = value;
        }
        values_.swap(new_values);
    }

    /**
     * Returns a copy of the sample's values.
     */
    std::vector<IntType> values() const
    {
        std::vector<IntType> v;
        for (typename std::map<double, IntType>::const_iterator 
                 it=values_.begin();
             it != values_.end();
             ++it)
        {
            v.push_back(it->second);
        }
        return v;
    }

private:
    long tick() const 
    {
        return time(NULL);
    }

   double next_random()
   {
       return dist_(gen_) / static_cast<double>(std::numeric_limits<IntType>::max());
   }
private:
    std::size_t size_;
    double alpha_;
    std::size_t count_;
    long start_time_;
    long next_scale_time_;
    std::map<double, IntType> values_;
    boost::random::uniform_int_distribution<IntType> dist_;
    boost::random::mt19937 gen_;
    const static long RESCALE_THRESHOLD = 60;
};


/**
 * Sliding sample of a stream of {@code long}s. Operates on fixed-time window,
 * expired points are simply dropped.
 */
template <typename IntType=unsigned long>
struct sliding_sample
{
    sliding_sample(std::size_t size, std::size_t width_in_ms)
        : size_(size),
          width_(width_in_ms * 1000),
          ticks_(size),
          values_(size)
    {
    }

    typedef uint64_t tick_t;

public:
    void clear()
    {
        ticks_.clear();
        values_.clear();
    }

    std::size_t size() const
    {
        return values_.size();
    }

    void update(IntType value)
    {
        tick_t ts = tick();
        cut(ts);
        values_.push_back(value);
        ticks_.push_back(ts);
    }

    std::vector<IntType> values() const
    {
        std::size_t expired = std::min(expired_before(tick()), size() - 1);
        return std::vector<IntType>(values_.begin() + expired, values_.end());
    }

private:
    tick_t tick() const
    {
        timeval tv;
        if (0 == gettimeofday(&tv, 0)) {
            return uint64_t(tv.tv_sec) * 1000000 + tv.tv_usec;
        }
        return 0;
    }

    void cut(tick_t ts)
    {
        std::size_t cut = expired_before(ts);
        if (cut) {
            values_.erase(values_.begin(), values_.begin() + cut);
            ticks_.erase(ticks_.begin(), ticks_.begin() + cut);
        }
    }

    std::size_t expired_before(tick_t ts) const {
        tick_t cutoff = ts - width_;
        tick_buffer::const_iterator it = ticks_.begin(), end = ticks_.end();
        while (it != end && *it < cutoff) {
            ++it;
        }
        return it - ticks_.begin();
    }

private:
    std::size_t size_;
    tick_t width_;
    typedef boost::circular_buffer<tick_t> tick_buffer;
    tick_buffer ticks_;
    boost::circular_buffer<IntType> values_;

};


/**
 * A random sample of a stream of {@code long}s. Uses Vitter's Algorithm R to
 * produce a statistically representative sample.
 *
 * @see <a href="http://www.cs.umd.edu/~samir/498/vitter.pdf">Random Sampling
 *      with a Reservoir</a>
 */
template <typename IntType=unsigned long>
struct uniform_sample 
{
    uniform_sample(std::size_t reservoir_size)
        : size_(reservoir_size),
          count_(0),
          values_(reservoir_size, 0)
    {
    }
public:
    void clear() 
    {
        std::fill_n(values_.begin(), size_, 0);
    }

    std::size_t size() const
    {
        return std::min(count_, size_);
    }

    void update(IntType value)
    {
        std::size_t c = ++count_;
        if (c <= size_) { values_[c-1] = value; }
        else
        {
            std::size_t r = next_random() % c;
            if (r < size_) { values_[r] = value; }
        }
    }

    std::vector<IntType> values() const
    {
        return std::vector<IntType>(values_.begin(), values_.begin()+size());
    }

private:
    IntType next_random() 
    {
        return dist_(gen_);
    }

private:
    std::size_t size_;
    std::size_t count_;
    std::vector<IntType> values_;
    boost::random::uniform_int_distribution<IntType> dist_;
    boost::random::mt19937 gen_;
};

#endif // include guard
