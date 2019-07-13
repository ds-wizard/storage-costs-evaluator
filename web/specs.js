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

exports.resultMappings = {
    'costsSetup': {
        '_classname': 'setup',
        '_total': ['operatorCosts'],
        'operator': 'operatorHours',
        'operatorCosts': 'operatorCosts'
    },
    'indicentResponse': {
        '_classname': 'incidentResponse',
        '_total': ['operatorCosts'],
        'frequency': 'frequency',
        'operatorCosts': 'operatorCosts'
    },
    'networking': {
        '_classname': 'networking',
        '_total': ['bandwidthCosts', 'firewallCosts', 'networkPorts', 'firewallSetup', 'firewallMaintenance'],
        'bandwidthBackup': 'bandwidthBackup',
        'bandwidthCosts': 'bandwidthCosts',
        'bandwidthOut': 'bandwidthOut',
        'firewalls': 'firewalls',
        'firewallCosts': 'firewallCosts',
        'firewallMaintenance': 'firewallMaintenance',
        'firewallSetup': 'firewallSetup',
        'networkPorts': 'networkPorts'
    },
    'powerSupplies': {
        '_classname': 'ups',
        '_total': ['totalCosts'],
        'totalCosts': 'totalCosts'
    },
    'storageDrives': {
        '_classname': 'storageDrives',
        '_total': ['driveCosts', 'powerCosts'],
        'backup': 'backup',
        'driveCosts': 'totalDriveCosts',
        'powerCosts': 'powerCosts',
        'rawVolume': 'rawVolume',
        'redundancy': 'redundancy',
        'replacements': 'replacements',
        'spaceNeeded': 'spaceNeeded',
        'usableVolume': 'volume'
    },
    'storageServers': {
        '_classname': 'storageServers',
        '_total': ['powerCosts', 'racksCosts', 'serversCosts'],
        'drives': 'drives',
        'powerCosts': 'powerCosts',
        'racks': 'racks',
        'racksCosts': 'racksCosts',
        'replacements': 'replacements',
        'servers': 'servers',
        'serversCosts': 'serversCosts',
        'shared': 'sharedServer'
    },
    'tapeBackup': {
        '_classname': 'tapeBackup',
        '_total': ['driveCosts', 'operatorCosts', 'robotCosts', 'tapeCosts'],
        'backupTime': 'backupTime',
        'dailyBackup': 'dailyBackup',
        'driveCosts': 'totalDriveCosts',
        'drives': 'drives',
        'operatorCosts': 'operatorCosts',
        'repairTime': 'acceptableRepairTime',
        'restoreTime': 'restoreTime',
        'robotCosts': 'totalRobotCosts',
        'sharedTape': 'sharedTape',
        'tapeCosts': 'totalTapeCosts',
        'tapeIndex': 'tapeIndex',
        'tapeOperator': 'tapeOperator',
        'tapeRobot': 'tapeRobot',
        'tapes': 'tapes'
    }
}
