exports.inputs = {
    'properties': {
        'id': 'inputParameters',
        'title': 'Input parameters',
        'icon': 'keyboard',
        'parts': [
            {
                'id': 'core',
                'title': '',
                'fields': [
                    {
                        'id': 'volume',
                        'title': 'Volume',
                        'range': {
                            'min': 0,
                            'max': 1000,
                        },
                        'type': 'number',
                        'default': 500,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'select',
                            'default': 'GB',
                            'options': [
                                {
                                    'value': 'GB',
                                    'title': 'GB'
                                },
                                {
                                    'value': 'TB',
                                    'title': 'TB'
                                },
                                {
                                    'value': 'PB',
                                    'title': 'PB'
                                }
                            ]
                        }
                    },
                    {
                        'id': 'lifetime',
                        'title': 'Lifetime',
                        'range': {
                            'min': 0,
                            'max': 50
                        },
                        'type': 'number',
                        'default': 10,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'years'
                        }
                    }
                ]
            },
            {
                'id':  'usage',
                'title': 'Usage',
                'fields': [
                    {
                        'id': 'dailyChanges',
                        'title': 'Daily changes',
                        'type': 'number',
                        'default': 10,
                        'inputParams': {
                            'min': 0,
                            'max': 100
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '%'
                        }
                    },
                    {
                        'id': 'contentType',
                        'title': 'Content type',
                        'type': 'select',
                        'default': 'ManySmallFiles',
                        'options': [
                            {
                                'value': 'ManySmallFiles',
                                'title': 'Many small files'
                            },
                            {
                                'value': 'FewLargeFiles',
                                'title': 'Few large files'
                            },
                            {
                                'value': 'Database',
                                'title': 'Database'
                            }
                        ]
                    },
                    {
                        'id': 'accessType',
                        'title': 'Access type',
                        'type': 'select',
                        'default': 'OneFileOnRequest',
                        'options': [
                            {
                                'value': 'OneFileOnRequest',
                                'title': 'One file on request'
                            },
                            {
                                'value': 'RemoteFiles',
                                'title': 'Remote files'
                            },
                            {
                                'value': 'DataBase',
                                'title': 'Database'
                            },
                            {
                                'value': 'WebSite',
                                'title': 'Website'
                            },
                            {
                                'value': 'CalculationServer',
                                'title': 'Calculation server'
                            },
                            {
                                'value': 'HighPerformance',
                                'title': 'High performance'
                            }
                        ]
                    },
                    {
                        'id': 'dailyReadVolume',
                        'title': 'Daily read volume',
                        'type': 'number',
                        'default': 10,
                        'inputParams': {
                            'min': 0,
                            'max': 100
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '%'
                        }
                    },
                ]
            },
            {
                'id': 'backup',
                'title': 'Backup',
                'fields': [
                    {
                        'id': 'tapeBackup',
                        'title': 'Tape backup',
                        'type': 'select',
                        'default': 'yes',
                        'options': [
                            {
                                'value': 'yes',
                                'title': 'Yes'
                            },
                            {
                                'value': 'no',
                                'title': 'No'
                            }
                        ]
                    },
                    {
                        'id': 'backupFrequency',
                        'title': 'Backup frequency',
                        'type': 'select',
                        'default': 'Days',
                        'options': [
                            {
                                'value': 'Hours',
                                'title': 'Hours'
                            },
                            {
                                'value': 'Days',
                                'title': 'Days'
                            },
                            {
                                'value': 'Weeks',
                                'title': 'Weeks'
                            },
                            {
                                'value': 'Months',
                                'title': 'Months'
                            }
                        ]
                    },
                    {
                        'id': 'backupHistory',
                        'title': 'Backup history',
                        'type': 'select',
                        'default': 'Months',
                        'options': [
                            {
                                'value': 'Hours',
                                'title': 'Hours'
                            },
                            {
                                'value': 'Days',
                                'title': 'Days'
                            },
                            {
                                'value': 'Weeks',
                                'title': 'Weeks'
                            },
                            {
                                'value': 'Months',
                                'title': 'Months'
                            },
                            {
                                'value': 'Years',
                                'title': 'Years'
                            }
                        ]
                    }
                ]
            },
            {
                'id': 'recovery',
                'title': 'Recovery',
                'fields': [
                    {
                        'id': 'repairWithin',
                        'title': 'Repair within',
                        'type': 'select',
                        'default': 'Hours',
                        'options': [
                            {
                                'value': 'Seconds',
                                'title': 'Seconds'
                            },
                            {
                                'value': 'Minutes',
                                'title': 'Minutes'
                            },
                            {
                                'value': 'Hours',
                                'title': 'Hours'
                            },
                            {
                                'value': 'Days',
                                'title': 'Days'
                            },
                            {
                                'value': 'Weeks',
                                'title': 'Weeks'
                            }
                        ]
                    },
                    {
                        'id': 'repairTimes',
                        'title': 'Repair times',
                        'type': 'select',
                        'default': 'D7H24',
                        'options': [
                            {
                                'value': 'D5H8',
                                'title': '8/5'
                            },
                            {
                                'value': 'D5H10',
                                'title': '10/5'
                            },
                            {
                                'value': 'D7H24',
                                'title': '24/7'
                            }
                        ],
                        'help': 'H/D = H hours per day, D days per week.'
                    },
                    {
                        'id': 'unavailability',
                        'title': 'Unavailability (per year)',
                        'type': 'select',
                        'default': 'Hour',
                        'options': [
                            {
                                'value': 'Hour',
                                'title': 'Hour'
                            },
                            {
                                'value': 'Day',
                                'title': 'Day'
                            },
                            {
                                'value': 'Week',
                                'title': 'Week'
                            }
                        ]
                    }
                ]
            },
            {
                'id': 'security',
                'title': 'Security',
                'fields': [
                    {
                        'id': 'securityLevel',
                        'title': 'Security level',
                        'category': 'security',
                        'type': 'select',
                        'default': 'OpenData',
                        'options': [
                            {
                                'value': 'OpenData',
                                'title': 'Open data'
                            },
                            {
                                'value': 'ClosedData',
                                'title': 'Closed data'
                            },
                            {
                                'value': 'PrivacySensitive',
                                'title': 'Privacy sensitive'
                            }
                        ]
                    },
                    {
                        'id': 'whoCanAccess',
                        'title': 'Who can access',
                        'category': 'security',
                        'type': 'select',
                        'default': 'OpenAccess',
                        'options': [
                            {
                                'value': 'OnlyMe',
                                'title': 'Only me'
                            },
                            {
                                'value': 'LocalCollaborators',
                                'title': 'Local collaborators'
                            },
                            {
                                'value': 'AuthorizedPeople',
                                'title': 'Authorized people'
                            },
                            {
                                'value': 'OpenAccess',
                                'title': 'Open access'
                            }
                        ]
                    },
                    {
                        'id': 'sharedInfrastracture',
                        'title': 'Shared infrastructure',
                        'category': 'security',
                        'type': 'select',
                        'default': 'yes',
                        'options': [
                            {
                                'value': 'yes',
                                'title': 'Yes'
                            },
                            {
                                'value': 'no',
                                'title': 'No'
                            }
                        ]
                    }
                ]
            }
        ],
        'fields': [
            {
                'id': 'volume',
                'title': 'Volume',
                'category': 'core',
                'type': 'number',
                'default': 0,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'select',
                    'default': 'TB',
                    'options': [
                        {
                            'value': 'GB',
                            'title': 'GB'
                        },
                        {
                            'value': 'TB',
                            'title': 'TB'
                        },
                        {
                            'value': 'PB',
                            'title': 'PB'
                        }
                    ]
                }
            },
            {
                'id': 'lifetime',
                'title': 'Lifetime',
                'category': 'core',
                'type': 'number',
                'default': 0,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'years'
                }
            },
            {
                'id': 'dailyChanges',
                'title': 'Daily changes',
                'category': 'usage',
                'type': 'number',
                'default': 10,
                'inputParams': {
                    'min': 0,
                    'max': 100
                },
                'unit': {
                    'type': 'simple',
                    'text': '%'
                }
            },
            {
                'id': 'contentType',
                'title': 'Content type',
                'category': 'usage',
                'type': 'select',
                'default': 'ManySmallFiles',
                'options': [
                    {
                        'value': 'ManySmallFiles',
                        'title': 'Many small files'
                    },
                    {
                        'value': 'FewLargeFiles',
                        'title': 'Few large files'
                    },
                    {
                        'value': 'Database',
                        'title': 'Database'
                    }
                ]
            },
            {
                'id': 'accessType',
                'title': 'Access type',
                'category': 'usage',
                'type': 'select',
                'default': 'OneFileOnRequest',
                'options': [
                    {
                        'value': 'OneFileOnRequest',
                        'title': 'One file on request'
                    },
                    {
                        'value': 'RemoteFiles',
                        'title': 'Remote files'
                    },
                    {
                        'value': 'Database',
                        'title': 'Database'
                    },
                    {
                        'value': 'Website',
                        'title': 'Website'
                    },
                    {
                        'value': 'CalculationServer',
                        'title': 'Calculation server'
                    },
                    {
                        'value': 'HighPerformance',
                        'title': 'High performance'
                    }
                ]
            },
            {
                'id': 'dailyReadVolume',
                'title': 'Daily read volume',
                'category': 'usage',
                'type': 'number',
                'default': 10,
                'inputParams': {
                    'min': 0,
                    'max': 100
                },
                'unit': {
                    'type': 'simple',
                    'text': '%'
                }
            },
            {
                'id': 'repairWithin',
                'title': 'Repair within',
                'category': 'recovery',
                'type': 'select',
                'default': 'Hours',
                'options': [
                    {
                        'value': 'Seconds',
                        'title': 'Seconds'
                    },
                    {
                        'value': 'Minutes',
                        'title': 'Minutes'
                    },
                    {
                        'value': 'Hours',
                        'title': 'Hours'
                    },
                    {
                        'value': 'Days',
                        'title': 'Days'
                    },
                    {
                        'value': 'Weeks',
                        'title': 'Weeks'
                    }
                ]
            },
            {
                'id': 'repairTimes',
                'title': 'Repair times',
                'category': 'recovery',
                'type': 'select',
                'default': 'D7H24',
                'options': [
                    {
                        'value': 'D5H8',
                        'title': '8/5'
                    },
                    {
                        'value': 'D5H10',
                        'title': '10/5'
                    },
                    {
                        'value': 'D7H24',
                        'title': '24/7'
                    }
                ],
                'help': 'H/D = H hours per day, D days per week.'
            },
            {
                'id': 'tapeBackup',
                'title': 'Tape backup',
                'category': 'backup',
                'type': 'select',
                'default': 'yes',
                'options': [
                    {
                        'value': 'yes',
                        'title': 'Yes'
                    },
                    {
                        'value': 'no',
                        'title': 'No'
                    }
                ]
            },
            {
                'id': 'backupFrequency',
                'title': 'Backup frequency',
                'category': 'backup',
                'type': 'select',
                'default': 'Days',
                'options': [
                    {
                        'value': 'Hours',
                        'title': 'Hours'
                    },
                    {
                        'value': 'Days',
                        'title': 'Days'
                    },
                    {
                        'value': 'Weeks',
                        'title': 'Weeks'
                    },
                    {
                        'value': 'Months',
                        'title': 'Months'
                    }
                ]
            },
            {
                'id': 'backupHistory',
                'title': 'Backup history',
                'category': 'backup',
                'type': 'select',
                'default': 'Months',
                'options': [
                    {
                        'value': 'Hours',
                        'title': 'Hours'
                    },
                    {
                        'value': 'Days',
                        'title': 'Days'
                    },
                    {
                        'value': 'Weeks',
                        'title': 'Weeks'
                    },
                    {
                        'value': 'Months',
                        'title': 'Months'
                    },
                    {
                        'value': 'Years',
                        'title': 'Years'
                    }
                ]
            },
            {
                'id': 'unavailability',
                'title': 'Unavailability (per year)',
                'category': 'recovery',
                'type': 'select',
                'default': 'Hour',
                'options': [
                    {
                        'value': 'Hour',
                        'title': 'Hour'
                    },
                    {
                        'value': 'Day',
                        'title': 'Day'
                    },
                    {
                        'value': 'Week',
                        'title': 'Week'
                    }
                ]
            },
            {
                'id': 'securityLevel',
                'title': 'Security level',
                'category': 'security',
                'type': 'select',
                'default': 'OpenData',
                'options': [
                    {
                        'value': 'OpenData',
                        'title': 'Open data'
                    },
                    {
                        'value': 'ClosedData',
                        'title': 'Closed data'
                    },
                    {
                        'value': 'PrivacySensitive',
                        'title': 'Privacy sensitive'
                    }
                ]
            },
            {
                'id': 'whoCanAccess',
                'title': 'Who can access',
                'category': 'security',
                'type': 'select',
                'default': 'OpenAccess',
                'options': [
                    {
                        'value': 'OnlyMe',
                        'title': 'Only me'
                    },
                    {
                        'value': 'LocalCollaborators',
                        'title': 'Local collaborators'
                    },
                    {
                        'value': 'AuthorizedPeople',
                        'title': 'Authorized people'
                    },
                    {
                        'value': 'OpenAccess',
                        'title': 'Open access'
                    }
                ]
            },
            {
                'id': 'sharedInfrastracture',
                'title': 'Shared infrastructure',
                'category': 'security',
                'type': 'select',
                'default': 'yes',
                'options': [
                    {
                        'value': 'yes',
                        'title': 'Yes'
                    },
                    {
                        'value': 'no',
                        'title': 'No'
                    }
                ]
            }
        ]
    },
    'configuration': {
        'id': 'configuration',
        'title': 'Configuration',
        'icon': 'cogs',
        'parts': [
            {
                'id': 'general',
                'title': 'General',
                'fields': [
                    {
                        'id': 'electricityCost',
                        'title': 'Electricity cost',
                        'type': 'number',
                        'default': 0.14,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€ / kWh'
                        }
                    },
                    {
                        'id': 'powerEfficiency',
                        'title': 'Power efficiency',
                        'type': 'number',
                        'default': 70,
                        'inputParams': {
                            'min': 0,
                            'max': 100
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '%'
                        }
                    },
                    {
                        'id': 'costMHR',
                        'title': 'Cost of 1 manhour',
                        'type': 'number',
                        'default': 70,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€'
                        }
                    }
                ]
            },
            {
                'id': 'hdds',
                'title': 'HDDs',
                'fields': [
                    {
                        'id': 'hddCost',
                        'title': 'HDD cost',
                        'type': 'number',
                        'default': 30,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€ / TB'
                        }
                    },
                    {
                        'id': 'hddSize',
                        'title': 'HDD size',
                        'type': 'number',
                        'default': 10,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'TB'
                        }
                    },
                    {
                        'id': 'hddPower',
                        'title': 'HDD power consumption',
                        'type': 'number',
                        'default': 1.5,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'Watt / TB'
                        }
                    },
                    {
                        'id': 'lifetimeHDD',
                        'title': 'Lifetime HDD',
                        'type': 'number',
                        'default': 3,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'years'
                        }
                    }
                ]
            },
            {
                'id': 'servers',
                'title': 'Servers',
                'fields': [
                    {
                        'id': 'storageServer',
                        'title': 'Storage server cost',
                        'type': 'number',
                        'default': 5000,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€'
                        }
                    },
                    {
                        'id': 'storageRack',
                        'title': 'Storage rack cost',
                        'type': 'number',
                        'default': 2000,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€ / year'
                        }
                    },
                    {
                        'id': 'serverPower',
                        'title': 'Server power consumption',
                        'type': 'number',
                        'default': 200,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'Watt'
                        }
                    },
                    {
                        'id': 'lifetimeServer',
                        'title': 'Lifetime server',
                        'type': 'number',
                        'default': 5,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'years'
                        }
                    },
                    {
                        'id': 'ups',
                        'title': 'UPS cost',
                        'type': 'number',
                        'default': 0.2,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€ / Watt / month'
                        }
                    }
                ]
            },
            {
                'id': 'backupTapes',
                'title': 'Backup tapes',
                'fields': [
                    {
                        'id': 'tapeSpeed',
                        'title': 'Tape speed',
                        'type': 'number',
                        'default': 432,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'GB / hour'
                        }
                    },
                    {
                        'id': 'tapeCapacity',
                        'title': 'Tape capacity',
                        'type': 'number',
                        'default': 15,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'TB'
                        }
                    },
                    {
                        'id': 'tapeCost',
                        'title': 'Tape cost',
                        'type': 'number',
                        'default': 150,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€'
                        }
                    },
                    {
                        'id': 'tapeDriveCost',
                        'title': 'Tape drive cost',
                        'type': 'number',
                        'default': 3000,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€'
                        }
                    },
                    {
                        'id': 'tapeRobotCost',
                        'title': 'Tape robot cost',
                        'type': 'number',
                        'default': 5000,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€'
                        }
                    }
                ]
            },
            {
                'id': 'networking',
                'title': 'Networking',
                'fields': [
                    {
                        'id': 'networkCost',
                        'title': 'Network cost',
                        'type': 'number',
                        'default': 0.5,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€ / TB'
                        }
                    },
                    {
                        'id': 'networkPortRent',
                        'title': 'Network port rent cost',
                        'type': 'number',
                        'default': 10,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€ / month'
                        }
                    },
                    {
                        'id': 'firewallCost',
                        'title': 'Firewall cost',
                        'type': 'number',
                        'default': 1000,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': '€'
                        }
                    },
                    {
                        'id': 'firewallMaintenance',
                        'title': 'Firewall maintenance',
                        'type': 'number',
                        'default': 0.1,
                        'inputParams': {
                            'min': 0
                        },
                        'unit': {
                            'type': 'simple',
                            'text': 'hours / month'
                        }
                    }
                ]
            }
        ],
        'fields': [
            {
                'id': 'hddCost',
                'title': 'HDD cost',
                'type': 'number',
                'default': 30,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€ / TB'
                }
            },
            {
                'id': 'hddSize',
                'title': 'HDD size',
                'type': 'number',
                'default': 10,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'TB'
                }
            },
            {
                'id': 'hddPower',
                'title': 'HDD power consumption',
                'type': 'number',
                'default': 1.5,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'Watt / TB'
                }
            },
            {
                'id': 'electricityCost',
                'title': 'Electricity cost',
                'type': 'number',
                'default': 0.14,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€ / kWh'
                }
            },
            {
                'id': 'powerEfficiency',
                'title': 'Power efficiency',
                'type': 'number',
                'default': 70,
                'inputParams': {
                    'min': 0,
                    'max': 100
                },
                'unit': {
                    'type': 'simple',
                    'text': '%'
                }
            },
            {
                'id': 'storageServer',
                'title': 'Storage server cost',
                'type': 'number',
                'default': 5000,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€'
                }
            },
            {
                'id': 'storageRack',
                'title': 'Storage rack cost',
                'type': 'number',
                'default': 2000,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€ / year'
                }
            },
            {
                'id': 'serverPower',
                'title': 'Server power consumption',
                'type': 'number',
                'default': 200,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'Watt'
                }
            },
            {
                'id': 'lifetimeHDD',
                'title': 'Lifetime HDD',
                'type': 'number',
                'default': 3,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'years'
                }
            },
            {
                'id': 'lifetimeServer',
                'title': 'Lifetime server',
                'type': 'number',
                'default': 5,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'years'
                }
            },
            {
                'id': 'ups',
                'title': 'UPS cost',
                'type': 'number',
                'default': 0.2,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€ / Watt / month'
                }
            },
            {
                'id': 'tapeSpeed',
                'title': 'Tape speed',
                'type': 'number',
                'default': 432,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'GB / hour'
                }
            },
            {
                'id': 'tapeCapacity',
                'title': 'Tape capacity',
                'type': 'number',
                'default': 15,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'TB'
                }
            },
            {
                'id': 'tapeCost',
                'title': 'Tape cost',
                'type': 'number',
                'default': 150,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€'
                }
            },
            {
                'id': 'tapeDriveCost',
                'title': 'Tape drive cost',
                'type': 'number',
                'default': 3000,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€'
                }
            },
            {
                'id': 'tapeRobotCost',
                'title': 'Tape robot cost',
                'type': 'number',
                'default': 5000,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€'
                }
            },
            {
                'id': 'costMHR',
                'title': 'Cost of 1 manhour',
                'type': 'number',
                'default': 70,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€'
                }
            },
            {
                'id': 'networkCost',
                'title': 'Network cost',
                'type': 'number',
                'default': 0.5,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€ / TB'
                }
            },
            {
                'id': 'networkPortRent',
                'title': 'Network port rent cost',
                'type': 'number',
                'default': 10,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€ / month'
                }
            },
            {
                'id': 'firewallCost',
                'title': 'Firewall cost',
                'type': 'number',
                'default': 1000,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': '€'
                }
            },
            {
                'id': 'firewallMaintenance',
                'title': 'Firewall maintenance',
                'type': 'number',
                'default': 0.1,
                'inputParams': {
                    'min': 0
                },
                'unit': {
                    'type': 'simple',
                    'text': 'hours / month'
                }
            }
        ]
    }
}

exports.results = [
    {
        id: 'storageDrives',
        title: 'Storage drives',
        icon: 'hdd',
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
        icon: 'server',
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
        icon: 'network-wired',
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
        icon: 'database',
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
        icon: 'tools',
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
        icon: 'first-aid',
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
        icon: 'bolt',
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
