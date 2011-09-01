// -------------------------------------------------------------------
//
// basho_metrics:  fast performance metrics for Erlang.
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
#ifndef HISTOGRAM_HPP_
#define HISTOGRAM_HPP_

#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>
#include "sample.hpp"


template <typename IntType=unsigned long>
class histogram
{
public:
    histogram(std::size_t reservoir_size) 
    : sample_(reservoir_size), 
      min_(std::numeric_limits<IntType>::max()), 
      max_(std::numeric_limits<IntType>::min()), 
      sum_(0), 
      count_(0),
      variance_(-1, 0) { } 
public:
    void clear() 
    {
        sample_.clear();
        min_ = std::numeric_limits<IntType>::max(),
        max_ = std::numeric_limits<IntType>::min(),
        sum_ = 0;
        count_ = 0;
        variance_ = std::make_pair(-1, 0);
    }
        
    void update(IntType value)
    {
        ++count_;
        sample_.update(value);
        min_ = std::min(min_, value);
        max_ = std::max(max_, value);
        sum_ += value;
        update_variance(value);
    }

   double max() const 
   {
       if (count_ > 0) 
           return max_;
       return 0.0;
   }

   double min() const
   {
       if (count_ > 0) 
           return min_;
       return 0.0;
   }

   double mean() const
   {
       if (count_ > 0) 
           return sum_ / (double) count_;
       return 0.0;
   }

   double stddev() const 
   {
       if (count_ > 0) 
           return std::sqrt(variance());
       return 0.0;
   }

   IntType count() const 
   { 
       return count_; 
   }

   struct calc_percentile
   {
       calc_percentile(const std::vector<IntType>& values)
           : values_(values) { }

       double operator()(double percentile) const
       {
           double pos(percentile * (values_.size() + 1));
           if (pos < 1) return values_[0];
           if (pos >= values_.size()) return values_[values_.size()-1];
           double lower = values_[((int)pos)-1];
           double upper = values_[(int)pos];
           return lower+(pos-std::floor(pos))*(upper-lower);
       }
   private:
       const std::vector<IntType>& values_;
   };

    std::vector<double> percentiles(const std::vector<double> pvec)
    {
        std::vector<double> scores(pvec.size(), 0.0);
        if (count_)
        {
            std::vector<IntType> values = sample_.values();
            std::sort(values.begin(), values.end());
            std::transform(pvec.begin(), pvec.end(), 
                           scores.begin(), calc_percentile(values));
        }
        return scores;
    }
private:
    double variance() const
    {
        if (count_ <= 1) 
            return 0.0;
        return variance_.first / (count_- 1);
    }

    void update_variance(IntType value)
    {
        if (variance_.first == -1)
        {
            variance_.first = value;
            variance_.second = 0;
        }
        else
        {
            double oldM = variance_.first;
            double oldS = variance_.second;
            double newM = oldM + ((value - oldM) / count_);
            double newS = oldS + ((value - oldM) * (value - newM));
            variance_.first = newM;
            variance_.second = newS;
        }
    }
private:
    uniform_sample<IntType> sample_;
    IntType min_;
    IntType max_;
    IntType sum_;
    IntType count_;
    std::pair<double, double> variance_;
};


#endif // include guard

