// -------------------------------------------------------------------
// 
// basho_metrics_nifs:  fast performance metrics for erlang
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
#include "basho_metrics_nifs.h"
#include "histogram_metric.hpp"
#include "meter_metric.hpp"
#include <cmath>
#include <vector>
#include <cstring>

static ErlNifResourceType* histogram_RESOURCE;
static ErlNifResourceType* meter_RESOURCE;

static const unsigned long DEFAULT_RESERVOIR_SIZE = 1028;

struct meter_handle
{
    meter<> *p;
};

struct histogram_handle
{
    std::size_t size;
    histogram<> *p;
};

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_MIN;
static ERL_NIF_TERM ATOM_MAX;
static ERL_NIF_TERM ATOM_MEAN;
static ERL_NIF_TERM ATOM_MEDIAN;
static ERL_NIF_TERM ATOM_COUNT;
static ERL_NIF_TERM ATOM_P50;
static ERL_NIF_TERM ATOM_P95;
static ERL_NIF_TERM ATOM_P99;
static ERL_NIF_TERM ATOM_P999;
static ERL_NIF_TERM ATOM_ONE;
static ERL_NIF_TERM ATOM_FIVE;
static ERL_NIF_TERM ATOM_FIFTEEN;
static ERL_NIF_TERM ATOM_SIZE;
static ERL_NIF_TERM ATOM_STDDEV;

static ErlNifFunc nif_funcs[] =
{
    {"histogram_new", 1, histogram_new},
    {"histogram_update", 2, histogram_update},
    {"histogram_stats", 1, histogram_stats},
    {"histogram_clear", 1, histogram_clear},
    {"meter_new", 0, meter_new},
    {"meter_update", 2, meter_update},
    {"meter_tick", 1, meter_tick},
    {"meter_stats", 1, meter_stats},
};

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
#define STAT_TUPLE(Key, Value) enif_make_tuple2(env, Key, enif_make_ulong(env, static_cast<unsigned long>(Value)))

template <typename Acc> ERL_NIF_TERM fold(ErlNifEnv* env, ERL_NIF_TERM list,
                                          ERL_NIF_TERM(*fun)(ErlNifEnv*, ERL_NIF_TERM, Acc&),
                                          Acc& acc)
{
    ERL_NIF_TERM head, tail = list;
    while (enif_get_list_cell(env, tail, &head, &tail))
    {
        ERL_NIF_TERM result = fun(env, head, acc);
        if (result != ATOM_OK)
        {
            return result;
        }
    }
    return ATOM_OK;
}

ERL_NIF_TERM parse_histogram_option(ErlNifEnv* env, ERL_NIF_TERM item, 
                                    histogram_handle& handle)
{
    int arity;
    const ERL_NIF_TERM* option;
    if (enif_get_tuple(env, item, &arity, &option))
    {
        if (option[0] == ATOM_SIZE)
        {
            unsigned long sample_size;
            if (enif_get_ulong(env, option[1], &sample_size))
            {
                handle.size = sample_size;
            }
        }
    }
    return ATOM_OK;
}


ERL_NIF_TERM histogram_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    histogram_handle *handle = 
        (histogram_handle *)enif_alloc_resource(histogram_RESOURCE,
                                                sizeof(histogram_handle));
    if (enif_is_list(env, argv[0]))
    {
        memset(handle, '\0', sizeof(histogram_handle));
        handle->size = DEFAULT_RESERVOIR_SIZE;
        fold(env, argv[0], parse_histogram_option, *handle);
        handle->p = new histogram<>(handle->size);
        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource(handle);
        return enif_make_tuple2(env, ATOM_OK, result);
    }
    else 
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM histogram_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    histogram_handle* handle;
    if (enif_get_resource(env,argv[0],histogram_RESOURCE,(void**)&handle))
    {
        handle->p->clear();
        return ATOM_OK;
    }
    else 
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM histogram_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    histogram_handle* handle;
    unsigned long sample;
    if (enif_get_resource(env,argv[0],histogram_RESOURCE,(void**)&handle) &&
        enif_get_ulong(env, argv[1], &sample)) 
    {
        handle->p->update(sample);
        return ATOM_OK;
    }
    else 
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM histogram_stats(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    histogram_handle* handle;
    if (enif_get_resource(env,argv[0],histogram_RESOURCE,(void**)&handle))
    {
        std::vector<double> percentiles;
        percentiles.push_back(0.500);
        percentiles.push_back(0.950);
        percentiles.push_back(0.990);
        std::vector<double> scores(handle->p->percentiles(percentiles));
        return enif_make_list8(env, 
                               STAT_TUPLE(ATOM_MIN, handle->p->min()),
                               STAT_TUPLE(ATOM_MAX, handle->p->max()),
                               STAT_TUPLE(ATOM_MEAN, handle->p->mean()),
                               STAT_TUPLE(ATOM_COUNT, handle->p->count()),
                               STAT_TUPLE(ATOM_STDDEV, handle->p->stddev()),
                               STAT_TUPLE(ATOM_P50, scores[0]),
                               STAT_TUPLE(ATOM_P95, scores[1]),
                               STAT_TUPLE(ATOM_P99, scores[2]));

    }
    else 
        return enif_make_badarg(env);
}

ERL_NIF_TERM meter_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    meter_handle *handle = 
        (meter_handle *)enif_alloc_resource(meter_RESOURCE,
                                            sizeof(meter_handle));
    memset(handle, '\0', sizeof(meter_handle));
    handle->p = new meter<>;
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, ATOM_OK, result);
}

ERL_NIF_TERM meter_tick(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    meter_handle *handle;
    if (enif_get_resource(env,argv[0],meter_RESOURCE,(void**)&handle))
    {
        handle->p->tick();
        return ATOM_OK;
    }
    else 
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM meter_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    meter_handle* handle;
    unsigned long sample;
    if (enif_get_resource(env,argv[0],meter_RESOURCE,(void**)&handle) &&
        enif_get_ulong(env, argv[1], &sample)) 
    {
        handle->p->mark(sample);
        return ATOM_OK;
    }
    else 
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM meter_stats(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    meter_handle* handle;
    if (enif_get_resource(env,argv[0],meter_RESOURCE,(void**)&handle))
    {
        return enif_make_list4(env, 
                               enif_make_tuple2(env,ATOM_COUNT, 
                                                enif_make_ulong(env, handle->p->count())),
                               enif_make_tuple2(env,ATOM_ONE,
                                                enif_make_double(env,handle->p->one())),
                               enif_make_tuple2(env,ATOM_FIVE,enif_make_double(env, handle->p->five())),
                               enif_make_tuple2(env,ATOM_FIFTEEN,enif_make_double(env, handle->p->fifteen())));
    }
    else 
        return enif_make_badarg(env);
}

static void histogram_resource_cleanup(ErlNifEnv* env, void* arg)
{
    histogram_handle* handle = (histogram_handle*)arg;
    delete handle->p;
}

static void meter_resource_cleanup(ErlNifEnv* env, void* arg)
{
    meter_handle* handle = (meter_handle*)arg;
    delete handle->p;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)
        (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    histogram_RESOURCE = enif_open_resource_type(env, 
                                                 NULL, 
                                                 "histogram_resource",
                                                 &histogram_resource_cleanup,
                                                 flags, 
                                                 NULL);
    meter_RESOURCE = enif_open_resource_type(env, 
                                             NULL, 
                                             "meter_resource",
                                             &meter_resource_cleanup,
                                             flags, 
                                             NULL);
    // Initialize common atoms
    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error");
    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");
    ATOM(ATOM_MIN, "min");
    ATOM(ATOM_MAX, "max");
    ATOM(ATOM_MEAN, "mean");
    ATOM(ATOM_MEDIAN, "median");
    ATOM(ATOM_COUNT, "count");
    ATOM(ATOM_P50, "p50");
    ATOM(ATOM_P95, "p95");
    ATOM(ATOM_P99, "p99");
    ATOM(ATOM_P999, "p999");
    ATOM(ATOM_ONE, "one");
    ATOM(ATOM_FIVE, "five");
    ATOM(ATOM_FIFTEEN, "fifteen");
    ATOM(ATOM_SIZE, "size");
    ATOM(ATOM_STDDEV, "stddev");
    return 0;
}

extern "C" {
    ERL_NIF_INIT(basho_metrics_nifs, nif_funcs, &on_load, NULL, NULL, NULL);
}
