// TODO: inputs to generate as well

exports.results = [
    {
        id: 'storageDrives',
        title: 'Storage drives',
        parts: [
            {
                'id': 'volume',
                'title': 'Volume',
                'type': 'value',
                'unit': 'TB'
            },
            {
                'id': 'backup',
                'title': 'Backup',
                'type': 'value',
                'unit': 'TB'
            },
            {
                'id': 'redundancy',
                'title': 'Redundancy',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'rawVolume',
                'title': 'Raw volume',
                'type': 'value',
                'unit': 'TB'
            },
            {
                'id': 'spaceNeeded',
                'title': 'Space needed',
                'type': 'value',
                'unit': 'TB'
            },
            {
                'id': 'replacements',
                'title': 'Replacements',
                'type': 'value',
                'unit': '# drive(s)'
            },
            {
                'id': 'totalDriveCosts',
                'title': 'Total drive costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'powerCosts',
                'title': 'Power costs',
                'type': 'subtotal',
                'unit': '€'
            }
        ]
    },
    {
        id: 'storageServers',
        title: 'Storage servers',
        parts: [
            {
                'id': 'drives',
                'title': '# of drives',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'sharedServer',
                'title': 'Shared server',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'servers',
                'title': '# of servers',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'racks',
                'title': '# of racks',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'replacements',
                'title': 'Replacements',
                'type': 'value',
                'unit': '# server(s)'
            },
            {
                'id': 'serversCosts',
                'title': 'Servers costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'racksCosts',
                'title': 'Racks costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'powerCosts',
                'title': 'Power costs',
                'type': 'subtotal',
                'unit': '€'
            }
        ]
    },
    {
        id: 'networking',
        title: 'Networking',
        parts: [
            {
                'id': 'firewalls',
                'title': 'Firewalls',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'bandwidthBackup',
                'title': 'Bandwidth backup',
                'type': 'value',
                'unit': 'TB'
            },
            {
                'id': 'bandwidthOut',
                'title': 'Bandwidth out',
                'type': 'value',
                'unit': 'TB / day'
            },
            {
                'id': 'networkPorts',
                'title': 'Network ports costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'bandwidthCosts',
                'title': 'Bandwidth costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'firewallCosts',
                'title': 'Firewall costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'firewallSetup',
                'title': 'Firewall setup costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'firewallMaintenance',
                'title': 'Firewall maintenance costs',
                'type': 'subtotal',
                'unit': '€'
            }
        ]
    },
    {
        id: 'tapeBackup',
        title: 'Tape backup',
        parts: [
            {
                'id': 'restoreTime',
                'title': 'Restore time',
                'type': 'value',
                'unit': 'hours'
            },
            {
                'id': 'acceptableRepairTime',
                'title': 'Acceptable repair time',
                'type': 'value',
                'unit': 'hours'
            },
            {
                'id': 'sharedTape',
                'title': 'Shared tape',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'dailyBackup',
                'title': 'Daily backup',
                'type': 'value',
                'unit': 'TB'
            },
            {
                'id': 'backupTime',
                'title': 'Backup time',
                'type': 'value',
                'unit': 'hours'
            },
            {
                'id': 'tapes',
                'title': '# of tapes',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'drives',
                'title': '# of drives',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'tapeRobot',
                'title': 'Tape robot',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'tapeOperator',
                'title': 'Tape operator',
                'type': 'value',
                'unit': 'hours'
            },
            {
                'id': 'tapeIndex',
                'title': 'Tape index',
                'type': 'value',
                'unit': ''
            },
            {
                'id': 'totalTapeCosts',
                'title': 'Tape costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'totalDriveCosts',
                'title': 'Drive costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'totalRobotCosts',
                'title': 'Robot costs',
                'type': 'subtotal',
                'unit': '€'
            },
            {
                'id': 'operatorCosts',
                'title': 'Operator costs',
                'type': 'subtotal',
                'unit': '€'
            }
        ]
    },
    {
        id: 'setup',
        title: 'Setup',
        parts: [
            {
                'id': 'operatorHours',
                'title': 'Operator hours',
                'type': 'value',
                'unit': 'hour(s)'
            },
            {
                'id': 'operatorCosts',
                'title': 'Operator costs',
                'type': 'subtotal',
                'unit': '€'
            }
        ]
    },
    {
        id: 'incidentResponse',
        title: 'Incident response',
        parts: [
            {
                'id': 'frequency',
                'title': 'Frequency',
                'type': 'value',
                'unit': 'per year'
            },
            {
                'id': 'operatorCosts',
                'title': 'Operator costs',
                'type': 'subtotal',
                'unit': '€'
            }
        ]
    },
    {
        id: 'ups',
        title: 'Uninterruptible power supplies',
        parts: [
            {
                'id': 'totalCosts',
                'title': 'UPS total costs',
                'type': 'subtotal',
                'unit': '€'
            }
        ]
    }
]

function transformYesNo(value) {
    return (value === true ? 'Yes' : (value === false ? 'No' : value));
}

function transformRound2(value) {
    return Math.round(value * 100) / 100;
}

function transformShow2Decimal(value) {
    return Number(value).toFixed(2);
}

exports.resultMappings = {
    'costsSetup': {
        '_classname': 'setup',
        '_total': ['operatorCosts'],
        'operator': {
            '_classname': 'operatorHours',
            '_transform': [transformRound2]
        },
        'operatorCosts': {
            '_classname': 'operatorCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        }
    },
    'indicentResponse': {
        '_classname': 'incidentResponse',
        '_total': ['operatorCosts'],
        'frequency': {
            '_classname': 'frequency',
            '_transform': [transformRound2]
        },
        'operatorCosts': {
            '_classname': 'operatorCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        }
    },
    'networking': {
        '_classname': 'networking',
        '_total': ['bandwidthCosts', 'firewallCosts', 'networkPorts', 'firewallSetup', 'firewallMaintenance'],
        'bandwidthBackup': {
            '_classname': 'bandwidthBackup',
            '_transform': []
        },
        'bandwidthCosts': {
            '_classname': 'bandwidthCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'bandwidthOut': {
            '_classname': 'bandwidthOut',
            '_transform': []
        },
        'firewalls': {
            '_classname': 'firewalls',
            '_transform': [transformYesNo]
        },
        'firewallCosts': {
            '_classname': 'firewallCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'firewallMaintenance': {
            '_classname': 'firewallMaintenance',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'firewallSetup': {
            '_classname': 'firewallSetup',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'networkPorts': {
            '_classname': 'networkPorts',
            '_transform': [transformRound2, transformShow2Decimal]
        }
    },
    'powerSupplies': {
        '_classname': 'ups',
        '_total': ['totalCosts'],
        'totalCosts': {
            '_classname': 'totalCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        }
    },
    'storageDrives': {
        '_classname': 'storageDrives',
        '_total': ['driveCosts', 'powerCosts'],
        'backup': {
            '_classname': 'backup',
            '_transform': []
        },
        'driveCosts': {
            '_classname': 'totalDriveCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'powerCosts': {
            '_classname': 'powerCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'rawVolume': {
            '_classname': 'rawVolume',
            '_transform': []
        },
        'redundancy': {
            '_classname': 'redundancy',
            '_transform': []
        },
        'replacements': {
            '_classname': 'replacements',
            '_transform': [transformRound2]
        },
        'spaceNeeded': {
            '_classname': 'spaceNeeded',
            '_transform': []
        },
        'usableVolume': {
            '_classname': 'volume',
            '_transform': []
        }
    },
    'storageServers': {
        '_classname': 'storageServers',
        '_total': ['powerCosts', 'racksCosts', 'serversCosts'],
        'drives': {
            '_classname': 'drives',
            '_transform': []
        },
        'powerCosts': {
            '_classname': 'powerCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'racks': {
            '_classname': 'racks',
            '_transform': [transformRound2]
        },
        'racksCosts': {
            '_classname': 'racksCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'replacements': {
            '_classname': 'replacements',
            '_transform': []
        },
        'servers': {
            '_classname': 'servers',
            '_transform': []
        },
        'serversCosts': {
            '_classname': 'serversCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'shared': {
            '_classname': 'sharedServer',
            '_transform': [transformYesNo]
        }
    },
    'tapeBackup': {
        '_classname': 'tapeBackup',
        '_total': ['driveCosts', 'operatorCosts', 'robotCosts', 'tapeCosts'],
        'backupTime': {
            '_classname': 'backupTime',
            '_transform': [transformRound2]
        },
        'dailyBackup': {
            '_classname': 'dailyBackup',
            '_transform': []
        },
        'driveCosts': {
            '_classname': 'totalDriveCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'drives': {
            '_classname': 'drives',
            '_transform': [transformRound2]
        },
        'operatorCosts': {
            '_classname': 'operatorCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'repairTime': {
            '_classname': 'acceptableRepairTime',
            '_transform': []
        },
        'restoreTime': {
            '_classname': 'restoreTime',
            '_transform': [transformRound2]
        },
        'robotCosts': {
            '_classname': 'totalRobotCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'sharedTape': {
            '_classname': 'sharedTape',
            '_transform': [transformYesNo]
        },
        'tapeCosts': {
            '_classname': 'totalTapeCosts',
            '_transform': [transformRound2, transformShow2Decimal]
        },
        'tapeIndex': {
            '_classname': 'tapeIndex',
            '_transform': [transformYesNo]
        },
        'tapeOperator': {
            '_classname': 'tapeOperator',
            '_transform': [transformRound2]
        },
        'tapeRobot': {
            '_classname': 'tapeRobot',
            '_transform': [transformYesNo]
        },
        'tapes': {
            '_classname': 'tapes',
            '_transform': [transformRound2]
        }
    }
}
