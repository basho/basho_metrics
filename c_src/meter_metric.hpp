// -------------------------------------------------------------------
//
// basho_metrics:  fast performance metrics for erlang
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

template <typename IntType=unsigned long>
class meter
{
public:
    meter()
        : count_(0),
          one_(alpha_one()),
          five_(alpha_five()),
          fifteen_(alpha_fifteen()) { } 

    void tick()
    {
        one_.tick();
        five_.tick();
        fifteen_.tick();
    }

    void mark()
    {
        mark(1);
    }

    void mark(IntType n)
    {
        count_ += n;
        one_.update(n);
        five_.update(n);
        fifteen_.update(n);
    }

    unsigned long count() const 
    {
        return count_;
    }
    
    double one() const 
    {
        return one_.rate();
    }

    double five() const
    {
        return five_.rate();
    }

    double fifteen() const
    {
        return fifteen_.rate();
    }
private:
    unsigned long count_;
    ewma one_;
    ewma five_;
    ewma fifteen_;
    long start_time_;
};


#endif // include guard
