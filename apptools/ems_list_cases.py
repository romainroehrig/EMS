#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import logging
logging.basicConfig(format='%(asctime)s - %(name)30s - %(levelname)s - %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

from ems.cases import available

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description="Provide information about cases/subcases available in EMS")
    parser.add_argument('-c', '--case', dest='case', required=False, default=None,
                        help="Case name for which you want to list available subcases")
    args = parser.parse_args()
    available(case=args.case)
