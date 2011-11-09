#!/bin/bash

echo "start `date`"

echo "/home/balaji/ir_cpp/bin/x_text_classifier_training_data /home/balaji/ir_cpp/configs/channels_classifier.config /home/balaji/ir_cpp/configs/keytuples_extracter.config /home/balaji/ir_cpp/data/static_data/classifier_dictionary.txt > /home/balaji/ir_cpp/log/x_text_classifier_training_data.channels.log 2>&1"
/home/balaji/ir_cpp/bin/x_text_classifier_training_data /home/balaji/ir_cpp/configs/channels_classifier.config /home/balaji/ir_cpp/configs/keytuples_extracter.config /home/balaji/ir_cpp/data/static_data/classifier_dictionary.txt > /home/balaji/ir_cpp/log/x_text_classifier_training_data.channels.log 2>&1

echo "/home/balaji/ir_cpp/bin/x_make_nbc_probabilities_file /home/balaji/ir_cpp/configs/iab_classifier.config 0"
/home/balaji/ir_cpp/bin/x_make_nbc_probabilities_file /home/balaji/ir_cpp/configs/iab_classifier.config 0

echo "/home/balaji/ir_cpp/bin/x_make_nbc_probabilities_file /home/balaji/ir_cpp/configs/location_classifier.config 0"
/home/balaji/ir_cpp/bin/x_make_nbc_probabilities_file /home/balaji/ir_cpp/configs/location_classifier.config 0

echo "end `date`"
