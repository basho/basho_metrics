// -------------------------------------------------------------------
//
// basho_metrics:  fast performance metrics for erlang
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
#ifndef METER_METRIC_HPP_
#define METER_METRIC_HPP_

#include "ewma.hpp"

/**
 * A meter metric which measures mean throughput and one-, five-, and
 * fifteen-minute exponentially-weighted moving average throughputs.
 *
 * http://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average
 */
template <typename IntType=unsigned long>
class meter
{
public:
    meter(unsigned int interval)
        : count_(0),
          one_(alpha_one(), interval),
          five_(alpha_five(), interval),
          fifteen_(alpha_fifteen(), interval) { } 


    /**
     * Updates the moving averages.
     */
    void tick()
    {
        one_.tick();
        five_.tick();
        fifteen_.tick();
    }

    /**
     * Mark the occurrence of an event.
     */
    void mark()
    {
        mark(1);
    }

    /**
     * Mark the occurrence of a given number of events.
     */
    void mark(IntType n)
    {
        count_ += n;
        one_.update(n);
        five_.update(n);
        fifteen_.update(n);
    }

    /**
     * Returns the number of events which have been marked.
     */
    unsigned long count() const 
    {
        return count_;
    }

    /**
     * Returns the one-minute exponentially-weighted moving average rate at
     * which events have occurred since the meter was created.
     *
     * This rate has the same exponential decay factor as the one-minute load
     * average in the 'top' Unix command.
     */    
    double one() const 
    {
        return one_.rate();
    }

    /**
     * Returns the five-minute exponentially-weighted moving average rate at
     * which events have occurred since the meter was created.

     * This rate has the same exponential decay factor as the five-minute load
     * average in the 'top' Unix command.
     */
    double five() const
    {
        return five_.rate();
    }

    /**
     * Returns the fifteen-minute exponentially-weighted moving average rate at
     * which events have occurred since the meter was created.
     * <p>
     * This rate has the same exponential decay factor as the fifteen-minute 
     * load average in the 'top' Unix command.
     */    
    double fifteen() const
    {
        return fifteen_.rate();
    }
private:
    unsigned long count_;
    ewma one_;
    ewma five_;
    ewma fifteen_;
    unsigned long start_time_;
};


#endif // include guard
