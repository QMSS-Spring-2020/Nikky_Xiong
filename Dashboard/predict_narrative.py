#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def predict_narrative(new_narrative):
    import pickle
    pipeline = pickle.load(open("model-dir/model_svc_fit.pkl", 'rb') )
    cat = pipeline.predict([new_narrative])
    return cat
