window.BENCHMARK_DATA = {
  "lastUpdate": 1781724468764,
  "repoUrl": "https://github.com/arthUFO12/jsip-exchange",
  "entries": {
    "Order book benchmark": [
      {
        "commit": {
          "author": {
            "email": "arthur.c.ufongene.28@dartmouth.edu",
            "name": "Arthur Ufongene",
            "username": "arthUFO12"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c18534d0e65eb65f7295bbd163bd0c777d410106",
          "message": "Merge branch 'jane-street-immersion-program:main' into main",
          "timestamp": "2026-06-17T15:23:49-04:00",
          "tree_id": "b105f708f1d0a3bfac0fc8f703926fc5cb5958f3",
          "url": "https://github.com/arthUFO12/jsip-exchange/commit/c18534d0e65eb65f7295bbd163bd0c777d410106"
        },
        "date": 1781724468188,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "find_match (n=10)",
            "value": 24.32540963637692,
            "unit": "ns"
          },
          {
            "name": "find_match (n=50)",
            "value": 24.446161283074566,
            "unit": "ns"
          },
          {
            "name": "find_match (n=100)",
            "value": 24.435535497719407,
            "unit": "ns"
          },
          {
            "name": "find_match (n=500)",
            "value": 24.528847233283244,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=10)",
            "value": 114.51363951973211,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=50)",
            "value": 511.63903468084493,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=100)",
            "value": 998.5435954468751,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=500)",
            "value": 4954.370520317935,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=10)",
            "value": 218.79122041864676,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=50)",
            "value": 1004.4802278824837,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=100)",
            "value": 1991.085445359495,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=500)",
            "value": 9714.162997465106,
            "unit": "ns"
          },
          {
            "name": "add+remove (n=100)",
            "value": 1345.0095452191974,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=10)",
            "value": 1229.8531089431826,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=50)",
            "value": 5148.519864156323,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=100)",
            "value": 10253.537854942348,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=500)",
            "value": 48831.38452789385,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=10)",
            "value": 597.7174079216946,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=50)",
            "value": 2571.323755528394,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=100)",
            "value": 5037.640681788398,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=500)",
            "value": 24441.207231801665,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_10_levels",
            "value": 5342.46869398142,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_50_levels",
            "value": 83567.95043970391,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_100_levels",
            "value": 323174.86378213234,
            "unit": "ns"
          },
          {
            "name": "find_match_alloc (n=100)",
            "value": 25.88169331470305,
            "unit": "ns"
          }
        ]
      }
    ]
  }
}