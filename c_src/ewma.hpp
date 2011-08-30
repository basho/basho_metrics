// -------------------------------------------------------------------
//
// basho_metrics:  fast performance metrics for Erlang.
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
#ifndef EWMA_HPP_
#define EWMA_HPP_

#include <cmath>

class ewma
{
    static const double NANOS = 1000000000.0;
public:
    explicit ewma(double alpha, long interval=5)
        : rate_(0.0),
          uncounted_(0),
          alpha_(alpha),
          interval_(interval*NANOS),
          initialized_(false) { } 

    void update(long n) 
    {
        uncounted_ += n;
    }

    void tick()
    {
        std::size_t count = uncounted_;
        uncounted_ = 0;
        double instant_rate = count / interval_;
        if (initialized_)
            rate_ += (alpha_ * (instant_rate - rate_));
        else 
        {
            rate_ = instant_rate;
            initialized_ = true;
        }
    }


    double rate() const
    {
        return rate_ * (double)NANOS;
    }

private:
    double rate_;
    unsigned int uncounted_;
    double alpha_;
    double interval_;
    bool initialized_;
};

inline double alpha_one()
{
    return 1.0 - std::exp(-5.0 / 60.0);
}

inline double alpha_five()
{
    return 1.0 - std::exp(-5.0 / 60.0 / 5.0);
}

inline double alpha_fifteen()
{
    return 1.0 - std::exp(-5.0 / 60.0 / 15.0);
}

#endif // include guard
