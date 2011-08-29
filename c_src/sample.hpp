// -------------------------------------------------------------------
//
// XXX
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
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

template <typename IntType=long>
struct exponentially_decaying_sample
{
    exponentially_decaying_sample(std::size_t size=1024, double alpha=0.015)
        : size_(size),
          alpha_(alpha),
          count_(0),
          start_time_(tick())
    {
    }
        
    void clear()
    {
        values_.clear();
        count_ = 0;
        start_time_ = tick();
        next_scale_time_ = start_time_ + RESCALE_THRESHOLD;
    }

    std::size_t size() const
    {
        return std::min(size_, count_);
    }

    double weight(long t) const
    {
        return std::exp(alpha_ * t);
    }

    void update(IntType value) 
    {
        update(value, tick());
    }

    void update(IntType value, long timestamp)
    {
        double priority = weight(timestamp - start_time_) / (float)(rand()/RAND_MAX);
        if (++count_ <= size_)
            values_[priority] = value;
        else
        {
            double first = values_.begin()->first;
            if (first < priority)
                if (values_.find(priority) == values_.end())
                    values_[priority] = value;
        }
        long now = tick();
        if (now > next_scale_time_)
            rescale(now, next_scale_time_);
    }

    void rescale(long now, long next) 
    {
        next_scale_time_ = now + RESCALE_THRESHOLD;
        long old_start_time = start_time_;
        start_time_ = tick();
        std::vector<double> keys;
        for (typename std::map<double, IntType>::iterator 
                 it=values_.begin();
             it != values_.end();
             ++it)
        {
            IntType value = it->second;
            values_[it->first * std::exp(-alpha_ * (start_time_ - old_start_time))] = value;
            values_.erase(it);
        }
    }

    const std::vector<IntType> values() 
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
private:
    std::size_t size_;
    double alpha_;
    std::size_t count_;
    long start_time_;
    long next_scale_time_;
    std::map<double, IntType> values_;
    const static long RESCALE_THRESHOLD = 60;
};


template <typename IntType=unsigned int>
struct uniform_sample 
{
    uniform_sample(std::size_t reservoir_size)
        : size_(reservoir_size),
          count_(0),
          values_(reservoir_size, 0.0)
    {
    }
public:
    void clear() 
    {
        std::fill_n(values_.begin(), size_, 0.0);
    }

    std::size_t size()
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

    const std::vector<IntType> values() 
    {
        return values_;
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
