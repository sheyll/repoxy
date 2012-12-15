#!/bin/sh

erl -sname rpx_remsh -setcookie repoxy -remsh repoxy@`hostname`
