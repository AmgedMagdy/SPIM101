import math
import PySimpleGUI as sg             # GUI Interface
import numpy as np                   # Math library
import matplotlib.pyplot as plt      # Plotting library



# VARS CONSTS:
_VARS = {'window': False,
         'fig_agg': False,
         'pltFig': False,
         }


# Theme for pyplot
plt.style.use('seaborn')
AppFont = ('Any 16', 12)
sg.theme('Default1')


# GUI Layout

# Tab 1 tests inputs
tests_layout = [
                [
                sg.Text("", size=(24, 1), font=("any16", 12)),
                sg.Text("Voltage [V]",justification='c', size=(10, 1), font=("any16", 12)),
                sg.Text("Current [Amp]", justification='c',  size=(11, 1), font=("any16", 12)),
                sg.Text("Power [W]", size=(10, 1), font=("any16", 12))],
                [sg.Text("‣ No load Test:", justification='l', size=(20, 1), font=("any16", 14)),
                sg.Input(key='Vnl', do_not_clear=True, size=(10, 1), justification="l"),
                sg.Input(key='Inl', do_not_clear=True, size=(10, 1), justification="l"),
                sg.Input(key='Pnl', do_not_clear=True, size=(10, 1), justification="l")],
                [sg.Text("‣ Standstill Test (main): ", justification='l',size=(20, 1), font=("any16", 14)),
                 sg.Input(key='Vsc1', do_not_clear=True, size=(10, 1), justification="l"),
                 sg.Input(key='Isc1', do_not_clear=True, size=(10, 1), justification="l"),
                 sg.Input(key='Psc1', do_not_clear=True, size=(10, 1), justification="l")],

                [sg.Text("‣ Standstill Test (Aux): ", justification='l',size=(20, 1), font=("any16", 14)),
                sg.Input(key='Vsc2', do_not_clear=True, size=(10, 1), justification="l"),
                sg.Input(key='Isc2', do_not_clear=True, size=(10, 1), justification="l"),
                sg.Input(key='Psc2', do_not_clear=True, size=(10, 1), justification="l")],

                [sg.Text("‣ DC Test (Main): ", justification='l',size=(20, 1), font=("any16", 14)),
                sg.Input(key='Vdc1', do_not_clear=True, size=(10, 1), justification="l"),
                sg.Input(key='Idc1', do_not_clear=True, size=(10, 1), justification="l")],

                [sg.Text("‣ DC Test (Aux): ", justification='l',size=(20, 1), font=("any16", 14)),
                sg.Input(key='Vdc2', do_not_clear=True, size=(10, 1), justification="l"),
                sg.Input(key='Idc2', do_not_clear=True, size=(10, 1), justification="l")]]

# Tab 2 tests inputs
direct_inputs = [[sg.Text("R1 Main [Ω]:", justification='left', size=(15, 1)), sg.Input(key='R1_main2', do_not_clear=True, size=(13, 1), justification="left" ),
                 sg.Text("R2\' [Ω]:", justification='left', size=(15, 1)), sg.Input(key='R2_main2', do_not_clear=True, size=(13, 1), justification="left" )],
                 [sg.Text("X1 Main [Ω]:", justification='left', size=(15, 1)), sg.Input(key='X1_main2', do_not_clear=True, size=(13, 1), justification="left"),
                 sg.Text("X2\' [Ω]:", justification='left', size=(15, 1)), sg.Input(key='X2_main2', do_not_clear=True, size=(13, 1), justification="left")],

                [sg.Text("R1 Aux [Ω]:", justification='left', size=(15, 1)), sg.Input(key='R1_aux2', do_not_clear=True, size=(13, 1), justification="left"),
                 sg.Text("R2\'' [Ω]:", justification='left', size=(15, 1)), sg.Input(key='R2_aux2', do_not_clear=True, size=(13, 1), justification="left")],
                 [sg.Text("X1 Aux [Ω]:", justification='left', size=(15, 1)), sg.Input(key='X1_aux2', do_not_clear=True, size=(13, 1), justification="left"),
                 sg.Text("X2\'' [Ω]:", justification='left', size=(15, 1)), sg.Input(key='X2_aux2', do_not_clear=True, size=(13, 1), justification="left")],

                [sg.Text("Prot [W]:", justification='left', size=(15, 1)),
                            sg.Input(key='Prot2', do_not_clear=True, size=(13, 1), justification="left"),
                 sg.Text("Xm [Ω]:", justification='left', size=(15, 1)),
                 sg.Input(key='Xm2', do_not_clear=True, size=(13, 1), justification="left")
                 ]
]


eqv_parameters_layout = [
                        [sg.Text("R1 Main [Ω]:", justification='left', size=(10, 1)), sg.Input(key='R1_main', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("R2\' [Ω]:", justification='left', size=(10, 1)), sg.Input(key='R2_main', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("X1 Main [Ω]:", justification='left', size=(10, 1)), sg.Input(key='X1_main', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("X2\' [Ω]:", justification='left', size=(10, 1)), sg.Input(key='X2_main', do_not_clear=True, size=(13, 1), justification="left", readonly=True)],

                        [sg.Text("R1 Aux [Ω]:", justification='left', size=(10, 1)), sg.Input(key='R1_aux', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("R2\'' [Ω]:", justification='left', size=(10, 1)), sg.Input(key='R2_aux', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("X1 Aux [Ω]:", justification='left', size=(10, 1)), sg.Input(key='X1_aux', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("X2\'' [Ω]:", justification='left', size=(10, 1)), sg.Input(key='X2_aux', do_not_clear=True, size=(13, 1), justification="left", readonly=True)],

                        [sg.Text("Zmain [Ω]:", justification='left', size=(10, 1)), sg.Input(key='Zmain', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("Zaux [Ω]:", justification='left', size=(10, 1)), sg.Input(key='Zaux', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("Turns Ratio:", justification='left', size=(10, 1)), sg.Input(key='a_ratio', do_not_clear=True, size=(13, 1), justification="left", readonly=True),
                         sg.Text("Xµ [Ω]:", justification='left', size=(10, 1)), sg.Input(key='XM', do_not_clear=True, size=(13, 1), justification="left", readonly=True)],
                        [sg.Text("Prot [W]:", justification='left', size=(10, 1)),
                            sg.Input(key='Prot', do_not_clear=True, size=(13, 1), justification="left", readonly=True)]]

parameters_layout = [[sg.Text("", justification='left', size=(20, 1))],
                    [sg.Text("Vrated [V]:", justification='left', size=(20, 1)), sg.Input(key='vs', do_not_clear=True, size=(10, 1), justification="l")],
                    [sg.Text("Rated Frequency [Hz]:", size=(20, 1)), sg.Input(key='fr', do_not_clear=True, size=(10, 1))],
                    [sg.Text("No. of Poles:", size=(20, 1)), sg.Input(key='p1', do_not_clear=True, size=(10, 1))],
                    [sg.Text("Prated [hp]:", size=(20, 1)), sg.Input(key='Pr', do_not_clear=True, size=(10, 1))],
                    [sg.Text("Speed [RPM]:", size=(20, 1)), sg.Input(key='n', do_not_clear=True, size=(10, 1))],
                    [sg.Text("", justification='left', size=(20, 1))]
                    ]

parameters_layout2 = [
                    [sg.Text("Vrated [V]:", justification='left', size=(20, 1)), sg.Input(key='vs2', do_not_clear=True, size=(10, 1), justification="l")],
                    [sg.Text("Rated Frequency [Hz]:", size=(20, 1)), sg.Input(key='fr2', do_not_clear=True, size=(10, 1))],
                    [sg.Text("No. of Poles:", size=(20, 1)), sg.Input(key='p12', do_not_clear=True, size=(10, 1))],
                    [sg.Text("Prated [hp]:", size=(20, 1)), sg.Input(key='Pr2', do_not_clear=True, size=(10, 1))],
                    [sg.Text("Speed [RPM]:", size=(20, 1)), sg.Input(key='n2', do_not_clear=True, size=(10, 1))],

                    ]


starting_parameters_layout = [[sg.Text("Radd (Radd = 2 * Rsc(Aux), if not specified) [Ω]:", justification='left', size=(39, 1)),
                               sg.Input(key='Radd_alt', do_not_clear=True, size=(10, 1), justification="left"),
                               sg.Text("Xadd (Xadd = 2 * Xsc(Aux), if not specified) [Ω]:", justification='left', size=(39, 1)),
                               sg.Input(key='Xadd_alt', do_not_clear=True, size=(10, 1), justification="left")]]
starting_parameters_layout2 = [[sg.Text("Radd (Radd = 2 * Rsc(Aux), if not specified) [Ω]:", justification='left', size=(39, 1)),
                               sg.Input(key='Radd_alt2', do_not_clear=True, size=(10, 1), justification="left"),
                               sg.Text("Xadd (Xadd = 2 * Xsc(Aux), if not specified) [Ω]:", justification='left', size=(39, 1)),
                               sg.Input(key='Xadd_alt2', do_not_clear=True, size=(10, 1), justification="left")]]
results_layout = [
                [sg.Text("im [Amp]:", justification='left', size=(10, 1)), sg.Input(key='im', do_not_clear=True, size=(10, 1), justification="left", readonly=True),
                 sg.Text("Pin [W]:", justification='left', size=(10, 1)), sg.Input(key='pin', do_not_clear=True, size=(10, 1), justification="left", readonly=True)],
                [sg.Text("Pcu1 [W]:", justification='left', size=(10, 1)), sg.Input(key='pcu1', do_not_clear=True, size=(10, 1), justification="left", readonly=True),
                 sg.Text("Pcu2 [W]:", justification='left', size=(10, 1)), sg.Input(key='pcu2', do_not_clear=True, size=(10, 1), justification="left", readonly=True)],
                [sg.Text("Pgf [W]:", justification='left', size=(10, 1)), sg.Input(key='pgf', do_not_clear=True, size=(10, 1), justification="left", readonly=True),
                 sg.Text("Pgb [W]:", justification='left', size=(10, 1)), sg.Input(key='pgb', do_not_clear=True, size=(10, 1), justification="left", readonly=True)],

                 [sg.Text("Pd [W]:", justification='left', size=(10, 1)), sg.Input(key='pd', do_not_clear=True, size=(10, 1), justification="left", readonly=True),
                 sg.Text("pf:", justification='left', size=(10, 1)), sg.Input(key='pf', do_not_clear=True, size=(10, 1), justification="left", readonly=True)],
                 [sg.Text("Pout [W]:", justification='left', size=(10, 1)), sg.Input(key='pout', do_not_clear=True, size=(10, 1), justification="left", readonly=True),
                 sg.Text("Efficiency %:", justification='left', size=(10, 1)), sg.Input(key='eff', do_not_clear=True, size=(10, 1), justification="left", readonly=True)],
                ]

results_layout2 = [
                    [sg.Text("im [Amp]:", justification='left', size=(15, 1)), sg.Input(key='im2', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                     sg.Text("Turns Ratio:", justification='left', size=(15, 1)), sg.Input(key='turns_2', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                    [sg.Text("Pin [W]:", justification='left', size=(15, 1)), sg.Input(key='pin2', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                     sg.Text("Pcu1 [W]:", justification='left', size=(15, 1)), sg.Input(key='pcu12', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                    [sg.Text("Pcu2 [W]:", justification='left', size=(15, 1)), sg.Input(key='pcu22', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                     sg.Text("Pgf [W]:", justification='left', size=(15, 1)), sg.Input(key='pgf2', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                    [sg.Text("Pgb [W]:", justification='left', size=(15, 1)), sg.Input(key='pgb2', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                     sg.Text("Pd [W]:", justification='left', size=(15, 1)), sg.Input(key='pd2', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                    [sg.Text("pf:", justification='left', size=(15, 1)), sg.Input(key='pf2', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                     sg.Text("Pout [W]:", justification='left', size=(15, 1)), sg.Input(key='pout2', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                    [sg.Text("Efficiency %:", justification='left', size=(15, 1)), sg.Input(key='eff2', do_not_clear=True, size=(15, 1), justification="left", readonly=True)]
                ]

starting_layout_R = [
                [sg.Text("Ist [Amp]:", justification='left', size=(10, 1)), sg.Input(key='Ist_R', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                 sg.Text("Tst [N.m]:", justification='left', size=(10, 1)), sg.Input(key='Tst_R', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                [sg.Text("", justification='left', size=(10, 1))]
]

starting_layout_R2 = [
                [sg.Text("Ist [Amp]:", justification='left', size=(7, 1)), sg.Input(key='Ist_R2', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                 sg.Text("Tst [N.m]:", justification='left', size=(7, 1)), sg.Input(key='Tst_R2', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                [sg.Text("", justification='left', size=(10, 1))]
]

starting_layout_C = [
                [sg.Text("Ist [Amp]:", justification='left', size=(10, 1)), sg.Input(key='Ist_C', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                 sg.Text("Tst [N.m]:", justification='left', size=(10, 1)), sg.Input(key='Tst_C', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                [sg.Text("", justification='left', size=(10, 1))]
]

starting_layout_C2 = [
                [sg.Text("Ist [Amp]:", justification='left', size=(7, 1)), sg.Input(key='Ist_C2', do_not_clear=True, size=(15, 1), justification="left", readonly=True),
                 sg.Text("Tst [N.m]:", justification='left', size=(7, 1)), sg.Input(key='Tst_C2', do_not_clear=True, size=(15, 1), justification="left", readonly=True)],
                [sg.Text("", justification='left', size=(10, 1))]]

starting_group = [[sg.Frame("Resistance starting", starting_layout_R, title_color='#B22222', key='Rst_frame', element_justification='l', expand_x=True)],
                 [sg.Frame("Capacitor starting", starting_layout_C, title_color='#B22222', key='Cst_frame', element_justification='l')]]

starting_group2 = [[sg.Frame("Resistance starting", starting_layout_R2, title_color='#B22222', key='Rst_frame2', element_justification='l', expand_x=True),
                 sg.Frame("Capacitor starting", starting_layout_C2, title_color='#B22222', key='Cst_frame2', element_justification='l')]]


buttons_col = [[sg.Button('Calculate', key='calc1', font=("any16", 14), size=(10, 1)), sg.Button('Clear', key='Clear1', font=("any16", 14), size=(10, 1)),
                sg.Button('Exit', font=("any16", 14), size=(10, 1))
                ]]
buttons_col2 = [[sg.Button('Calculate', key='calc2', font=("any16", 14), size=(10, 1)), sg.Button('Clear', key='Clear2', font=("any16", 14), size=(10, 1))]]

frame_group = [
                [sg.Frame("Input Parameters for machine's tests.", tests_layout, title_color='#B22222', element_justification='l', expand_x=True),
                 sg.Frame("Machine's Parameters", parameters_layout, title_color='#B22222', element_justification='l')],
                [sg.Frame("Input Parameters for Starting (optional)", starting_parameters_layout, title_color='#B22222', element_justification='l')],
                [sg.Column(buttons_col, justification='c')],
                [sg.Frame("Eqv Circuit Parameters.", eqv_parameters_layout, title_color='#B22222', element_justification='l')],
                [sg.Frame("Performance", results_layout, title_color='#B22222', key='performance_key'),
                 sg.Column(starting_group, key='starting_group')]
            ]

frame_group2 = [[sg.Frame("Input equivalent circuit parameters", direct_inputs, title_color='#B22222', element_justification='l'),
                 sg.Frame("Machine Rating", parameters_layout2, title_color='#B22222', element_justification='l',expand_x=True)],
                [sg.Frame("Input Parameters for Starting (optional)", starting_parameters_layout2, title_color='#B22222', element_justification='l')],
                [sg.Frame("Performance", results_layout2, title_color='#B22222', key='performance_key2', element_justification='c',expand_x=True)],
                [sg.Column(starting_group2, key='starting_group')],
                [sg.Text("", justification='left', size=(10, 1))],
                [sg.Column(buttons_col2, justification='c')],
                [sg.Text("", justification='left', size=(10, 1))]
                ]

tab_group = [
    [sg.TabGroup(
        [
         [sg.Tab('Single-Phase IM: Using Tests',frame_group),
          sg.Tab('Single-Phase IM: Direct Readings',frame_group2, element_justification='c')]
         ])]
]

_VARS['window'] = sg.Window('SPIM101',
                            tab_group,
                            finalize=True,
                            resizable=False,
                            location=(300, 0),
                            icon='progfiles\icon2.ico',
                            element_justification="center",
                            font=('Any 16', 12),
                            background_color='#C1CDCD',
                            )


def testing():
    # Main - DC
    try:
        R1_m = (float(values['Vdc1'])/float(values['Idc1']))
    except:
        R1_m = 0
    # Aux - DC
    try:
        R1_a = (float(values['Vdc2'])/float(values['Idc2']))
    except:
        R1_a = 0

    # Main - StandStill
    try:
        Vsc_m = float(values['Vsc1'])
        Isc_m = float(values['Isc1'])
        Rsc_m = float(values['Psc1']) / (Isc_m ** 2)

        Zsc_m = Vsc_m / Isc_m
        Xsc_m = math.sqrt((Zsc_m ** 2) - (Rsc_m ** 2))
        R2_m = Rsc_m - R1_m
        X1_m = Xsc_m / 2
        X2_m = X1_m
        Z_m = round(Rsc_m, 3) + (round(Xsc_m, 3) * 1j)
    except:
        Z_m, X2_m, X1_m, R2_m = 0, 0, 0, 0


    # Aux - StandStill
    try:
        Vsc_a = float(values['Vsc2'])
        Isc_a = float(values['Isc2'])
        Rsc_a = float(values['Psc2']) / (Isc_a ** 2)

        Zsc_a = Vsc_a / Isc_a
        Xsc_a = math.sqrt((Zsc_a ** 2) - (Rsc_a ** 2))
        R2_a = Rsc_a - R1_a
        X1_a = Xsc_a / 2
        X2_a = X1_a
        Z_a = round(Rsc_a, 3) + (round(Xsc_a, 3) * 1j)
    except:
        Z_a, R2_a, X1_a, X2_a = 0, 0, 0, 0


    # Eff turns ratio
    turns_ratio = math.sqrt(R2_a / R2_m)

    # NL - Test
    try:
        Vnl = float(values['Vnl'])
        Inl = float(values['Inl'])
        Pnl = float(values['Pnl'])

        Prot = Pnl - ((Inl ** 2) * (R1_m + (R2_m / 4)))
        Znl = Vnl / Inl
        Rnl = R1_m + (R2_m/4)
        Xnl = math.sqrt((Znl ** 2) - (Rnl ** 2))
        Xm = (Xnl - X1_m - (X2_m / 2)) * 2
        print(Rnl)
    except:
        Xm, Prot = 0, 0
    _VARS['window']['R1_main'].update(round(R1_m, 3))
    _VARS['window']['R2_main'].update(round(R2_m, 3))
    _VARS['window']['X1_main'].update(round(X1_m, 3))
    _VARS['window']['X2_main'].update(round(X2_m, 3))
    _VARS['window']['R1_aux'].update(round(R1_a, 3))
    _VARS['window']['R2_aux'].update(round(R2_a, 3))
    _VARS['window']['X1_aux'].update(round(X1_a, 3))
    _VARS['window']['X2_aux'].update(round(X2_a, 3))
    _VARS['window']['Zmain'].update(Z_m)
    _VARS['window']['Zaux'].update(Z_a)
    _VARS['window']['a_ratio'].update(round(turns_ratio, 3))
    _VARS['window']['XM'].update(round(Xm, 3))
    _VARS['window']['Prot'].update(round(Prot,3))

    # 3.1 Starting
    try:
        Vs = float(values['vs'])
        f = float(values['fr'])
        p = float(values['Pr'])
        n = float(values['n'])
        p = p * 746
    except ValueError:
        pass
    else:
        if values['p1'] != '':
            p1 = int(values['p1'])
            ns = (60 * f) / (p1 / 2)
        else:
            p_predicted = 60 * f / n
            ns_ceiled = 60 * f / math.ceil(p_predicted)
            ns_floored = 60 * f / math.floor(p_predicted)
            lst = [ns_ceiled, ns_floored]
            ns = lst[min(range(len(lst)), key=lambda i: abs(lst[i] - n))]
            p1 = 60 * f / ns
        if p1 % 2 != 0:
            raise Exception
        s = (ns - n) / ns
        zf = (((R2_m / (2 * s)) + (X2_m / 2) * 1j) * (Xm / 2) * 1j) / ((R2_m / ((2 * s))) + (X2_m / 2) * 1j + (Xm / 2) * 1j)
        rf = zf.real
        xf = zf.imag
        zb = (((R2_m / (2 * (2 - s))) + (X2_m / 2) * 1j) * (Xm / 2) * 1j) / ((R2_m / (2 * (2 - s))) + (X2_m / 2) * 1j + (Xm / 2) * 1j)
        rb = zb.real
        xb = zb.imag
        z1 = R1_m + (X1_m * 1j)
        zt = z1 + zb + zf

        # Q3: Starting Radd
        if values['Radd_alt'] == '':
            Radd = 2 * Rsc_a
        else:
            Radd = float(values['Radd_alt'])
        Im = Vs / Z_m
        Ia_R = Vs / (Z_a + Radd)
        Ist_R = Im + Ia_R
        Tst_R = (2 * turns_ratio * R2_m / (60 * math.pi)) * abs(Im) * abs(Ia_R) * math.sin(np.angle(Ia_R) - np.angle(Im))
        print(abs(Im), abs(Ia_R))
        # Q3: Starting Xadd
        if values['Xadd_alt'] == '':
            Xadd = 2 * Xsc_a
        else:
            Xadd = float(values['Xadd_alt'])

        Ia_C = Vs / (Z_a - (Xadd * 1j))
        Ist_C = Im + Ia_C
        Tst_C = (2 * turns_ratio * R2_m / (60 * math.pi)) * abs(Im) * abs(Ia_C) * math.sin(np.angle(Ia_C) - np.angle(Im))

        _VARS['window']['Ist_R'].update("{} ∠{}°".format(round(abs(Ist_R), 3), round(np.angle(Ist_R, deg=True), 3)))
        _VARS['window']['Tst_R'].update(round(Tst_R, 4))
        _VARS['window']['Ist_C'].update("{} ∠{}°".format(round(abs(Ist_C), 3), round(np.angle(Ist_C, deg=True), 3)))
        _VARS['window']['Tst_C'].update(round(Tst_C, 4))
        _VARS['window']['Rst_frame'].update('Resistance starting @ Radd = {} Ω'.format(round(Radd, 4)))
        _VARS['window']['Cst_frame'].update('Capacitance starting @ Xadd = {} Ω'.format(round(Xadd,4)))

        # Q4: Performance
        im = Vs / zt
        pf = math.cos(np.angle(im))
        if np.angle(im, deg=1) < 0:
            pf_state = 'Lag'
        elif np.angle(im, deg=1) > 0:
            pf_state = 'Lead'
        else:
            pf_state = 'Unity'
        pin = Vs * abs(im) * pf
        pcu1 = (abs(im) **2) * R1_m
        pgf = (abs(im) **2) * rf
        pgb = (abs(im) **2) * rb
        pcu2 = (s * pgf) + ((2 - s) * pgb)
        pd = (1 - s) * (pgf - pgb)
        pout = pd - Prot
        eff = (pout / pin) * 100

        _VARS['window']['im'].update(round(abs(im), 3))
        _VARS['window']['pout'].update(round(pout, 3))
        _VARS['window']['pcu1'].update(round(pcu1, 3))
        _VARS['window']['pgf'].update(round(pgf, 3))
        _VARS['window']['pgb'].update(round(pgb, 3))
        _VARS['window']['pcu2'].update(round(pcu2, 3))
        _VARS['window']['pin'].update(round(pin, 3))
        _VARS['window']['pd'].update(round(pd, 5))
        _VARS['window']['pf'].update("{} {}".format(round(pf, 4), pf_state))
        _VARS['window']['eff'].update(round(eff, 3))
        _VARS['window']['performance_key'].update('Performance @ n = {} rpm'.format(n))


def testing2():

    R1_m = float(values['R1_main2'])
    R2_m = float(values['R2_main2'])
    X1_m = float(values['X1_main2'])
    X2_m = float(values['X2_main2'])
    Prot = float(values['Prot2'])
    Xm = float(values['Xm2'])
    try:
        R1_a = float(values['R1_aux2'])
        R2_a = float(values['R2_aux2'])
        X1_a = float(values['X1_aux2'])
        X2_a = float(values['X2_aux2'])
        turns_ratio = math.sqrt(R2_a / R2_m)
        Z_a = (R1_a+R2_a) + ((X1_a+X2_a) * 1j)
    except ValueError:
        Z_a = 0
        pass
    Rmain = R1_m + R2_m
    Xmain = X1_m + X2_m
    Z_m = Rmain + Xmain * 1j
    Vs = float(values['vs2'])
    f = float(values['fr2'])
    p = float(values['Pr2'])
    n = float(values['n2'])
    p = p * 746
    if values['p12'] != '':
        p1 = int(values['p12'])
        ns = (60 * f) / (p1 / 2)
    else:
        p_predicted = 60 * f / n
        ns_ceiled = 60 * f / math.ceil(p_predicted)
        ns_floored = 60 * f / math.floor(p_predicted)
        lst = [ns_ceiled, ns_floored]
        ns = lst[min(range(len(lst)), key=lambda i: abs(lst[i] - n))]
        p1 = 60 * f / ns
    if p1 % 2 != 0:
        raise Exception
    s = (ns - n) / ns
    zf = (((R2_m / (2 * s)) + (X2_m / 2) * 1j) * (Xm / 2) * 1j) / ((R2_m / ((2 * s))) + (X2_m / 2) * 1j + (Xm / 2) * 1j)
    rf = zf.real
    xf = zf.imag
    zb = (((R2_m / (2 * (2 - s))) + (X2_m / 2) * 1j) * (Xm / 2) * 1j) / ((R2_m / (2 * (2 - s))) + (X2_m / 2) * 1j + (Xm / 2) * 1j)
    rb = zb.real
    xb = zb.imag
    z1 = R1_m + (X1_m * 1j)
    zt = z1 + zb + zf

    # Q3: Starting Xadd
    if values['Radd_alt'] == '':
        Radd = 2 * Z_a.real
    else:
        Radd = float(values['Radd_alt2'])
    Im = Vs / Z_m
    Ia_R = Vs / (Z_a + Radd)
    Ist_R = Im + Ia_R
    Tst_R = (2 * turns_ratio * R2_m / (60 * math.pi)) * abs(Im) * abs(Ia_R) * math.sin(np.angle(Ia_R) - np.angle(Im))
    print(abs(Im), abs(Ia_R))
    # Q3: Starting Xadd
    if values['Xadd_alt'] == '':
        Xadd = 2 * Z_a.imag
    else:
        Xadd = float(values['Xadd_alt2'])

    Ia_C = Vs / (Z_a - (Xadd * 1j))
    Ist_C = Im + Ia_C
    Tst_C = (2 * turns_ratio * R2_m / (60 * math.pi)) * abs(Im) * abs(Ia_C) * math.sin(np.angle(Ia_C) - np.angle(Im))

    _VARS['window']['Ist_R2'].update("{} ∠{}°".format(round(abs(Ist_R), 3), round(np.angle(Ist_R, deg=True), 3)))
    _VARS['window']['Tst_R2'].update(round(Tst_R, 4))
    _VARS['window']['Ist_C2'].update("{} ∠{}°".format(round(abs(Ist_C), 3), round(np.angle(Ist_C, deg=True), 3)))
    _VARS['window']['Tst_C2'].update(round(Tst_C, 4))
    _VARS['window']['Rst_frame2'].update('Resistance starting @ Radd = {} Ω'.format(round(Radd, 4)))
    _VARS['window']['Cst_frame2'].update('Capacitance starting @ Xadd = {} Ω'.format(round(Xadd, 4)))



    #Q4: Performance
    im = Vs / zt
    pf = math.cos(np.angle(im))
    if np.angle(im, deg=1) < 0:
        pf_state = 'Lag'
    elif np.angle(im, deg=1) > 0:
        pf_state = 'Lead'
    else:
        pf_state = 'Unity'
    pin = Vs * abs(im) * pf
    pcu1 = (abs(im) **2) * R1_m
    pgf = (abs(im) **2) * rf
    pgb = (abs(im) **2) * rb
    pcu2 = (s * pgf) + ((2 - s) * pgb)
    pd = (1 - s) * (abs(im) ** 2) * (rf - rb)
    pout = pd - Prot
    eff = (pout / pin) * 100

    _VARS['window']['im2'].update(round(abs(im), 3))
    _VARS['window']['turns_2'].update(round(abs(turns_ratio), 3))
    _VARS['window']['pout2'].update(round(pout, 3))
    _VARS['window']['pcu12'].update(round(pcu1, 3))
    _VARS['window']['pgf2'].update(round(pgf, 3))
    _VARS['window']['pgb2'].update(round(pgb, 3))
    _VARS['window']['pcu22'].update(round(pcu2, 3))
    _VARS['window']['pin2'].update(round(pin, 3))
    _VARS['window']['pd2'].update(round(pd, 5))
    _VARS['window']['pf2'].update("{} {}".format(round(pf, 4), pf_state))
    _VARS['window']['eff2'].update(round(eff, 3))


# MAIN LOOP
while True:
    event, values = _VARS['window'].read(timeout=200)
    try:
        if event == 'calc1':
            testing()
    except Exception:
            sg.Popup('Please make sure you\'ve entered the required machine specs/test results and selected even number of poles'
                     , title='Error', font=AppFont, location=(500,300))
    try:
        if event == 'calc2':
            testing2()
    except Exception:
            sg.Popup('Please make sure you\'ve entered the required machine specs/test results and selected even number of poles'
                     , title='Error', font=AppFont, location=(500,300))
    if event == sg.WIN_CLOSED or event == 'Exit':
        break
    if event == 'Clear1' or event == 'Clear2' :
        for i in values:
            _VARS['window'][i].update('')
            _VARS['window']['Rst_frame'].update('Resistance starting')
            _VARS['window']['Cst_frame'].update('Capacitance starting')
            _VARS['window']['performance_key'].update('Performance')
            _VARS['window']['Rst_frame2'].update('Resistance starting')
            _VARS['window']['Cst_frame2'].update('Capacitance starting')
_VARS['window'].close()

