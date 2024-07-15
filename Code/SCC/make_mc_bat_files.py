"""
Create .bat files to run a Monte Carlo analysis
"""

from pathlib import Path
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from scipy.stats import qmc, truncnorm, lognorm

# Deterministic run
deterministic_run = r"""
    cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"
            gams run_rice50x.gms --policy=bau --climate=cbsimple --workdir=results_ocean --nameout=ocean_damage_9999 --mod_ocean=1
            gams run_rice50x.gms --policy=bau --climate=cbsimple --workdir=results_ocean --nameout=ocean_damage_pulse_9999 --mod_ocean=1 --mod_emission_pulse=ocean_damage_9999
            gams run_rice50x.gms --policy=bau --climate=cbsimple --workdir=results_ocean --nameout=ocean_today_9999 --mod_ocean=1 --policy=simulation_tatm_exogen --climate_of_today=1
"""
with open(Path.cwd() / 'Data/SCC/tmp/mc_seed_9999.bat', 'w') as f:
    f.write(deterministic_run)


#%% Latin hypercube sampling
uniform_params = {
                  'prstp': [0.01, 0.02],
                  'elasmu': [1.1, 1.6],
                  'baseline': [0, 1],
                  'health_mu': [0.01, 0.2],
                  'health_eta': [0.01, 0.2]
                  }
normal_params = [
    'ocean_value_intercept_unm',
    'ocean_value_exp_unm',
    'ocean_value_intercept_nu',
    'ocean_value_exp_nu',
    'ocean_area_damage_coef',
    'ocean_consump_damage_coef',
    'ocean_health_damage_coef',
    'ocean_health_damage_intercept',
]
positive_normal_params = {
    'vsl_start': [7.4, 4.7],
    'theta': [0.21, 0.09],
    'ocean_income_elasticity': [0.79, 0.09]
}
lognormal_params = {
    'tcre': [0.5, 0.43]
}

n = 500  # Sample size

# Uniformly distributed parameters
sampler = qmc.LatinHypercube(d=len(uniform_params), seed=1234)
sample = sampler.random(n=n)
l_bounds = [x[0] for x in uniform_params.values()]
u_bounds = [x[1] for x in uniform_params.values()]
sample_scaled = qmc.scale(sample, l_bounds, u_bounds)
uniform_params_sample = sample_scaled
# Normally distributed parameters. Distribution truncated at +/- 3 SD
mean, std = 0, 1
clip_a, clip_b = -3, 3
a, b = (clip_a - mean) / std, (clip_b - mean) / std
lhd = qmc.LatinHypercube(d=len(normal_params), optimization="random-cd", seed=1234).random(n=n)
sample = truncnorm(a, b, loc=mean, scale=std).ppf(lhd)
normal_params_sample = sample
# Normally distributed parameters but truncated at 0 and +3 SD
positive_normal_params_values = np.array(list(positive_normal_params.values()))
mean, std = positive_normal_params_values[:,0], positive_normal_params_values[:,1]
a, b = -mean/std, 3
lhd = qmc.LatinHypercube(d=len(positive_normal_params), optimization="random-cd", seed=1234).random(n=n)
sample = truncnorm(a, 3, loc=mean, scale=std).ppf(lhd)
positive_normal_params_sample = sample
# Log-normally distributed params.
lognormal_params_values = np.array(list(lognormal_params.values()))
s, mu = lognormal_params_values[:, 0], lognormal_params_values[:, 1]
lhd = qmc.LatinHypercube(d=len(lognormal_params), optimization="random-cd", seed=1234).random(n=n)
sample = lognorm(s=s, scale=np.exp(mu)).ppf(lhd)
lognormal_params_sample = sample

# Combine
sample_df = pd.concat([
    pd.DataFrame(uniform_params_sample, columns=list(uniform_params.keys())),
    pd.DataFrame(normal_params_sample, columns=list(normal_params)),
    pd.DataFrame(positive_normal_params_sample, columns=list(positive_normal_params)),
    pd.DataFrame(lognormal_params_sample, columns=list(lognormal_params)),
], axis=1)
sample_df['baseline'] = 'ssp' + (sample_df['baseline']*5+0.5).round(0).astype(int).astype(str)

# Plot parameter distributions
sample_df.hist(figsize=(12,12), grid=False)
fig = plt.gcf()
fig.suptitle('MC parameter distribution')
plt.savefig(Path.cwd() / 'Figures/SCC/MC_distributions.png')
plt.show()

# ocean_theta_1 = ocean_theta_2 = theta
if 'theta' in sample_df.columns:
    sample_df['ocean_theta_1'] = sample_df['theta']
    sample_df['ocean_theta_2'] = sample_df['theta']
    sample_df = sample_df.drop(columns='theta')

l = [r'cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"']
for i, row in sample_df.iterrows():
    s = ' '.join(('--' + row.index + '=' + row.astype(str)).tolist())
    txt = f"""        
    IF EXIST "results_ocean\ocean_damage_{i}.gdx" (
            echo File exists.
        ) ELSE (
            echo not 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --climate=cbsimple --workdir=results_ocean --nameout=ocean_today_{i} --policy=simulation_tatm_exogen --climate_of_today=1 {s} 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --climate=cbsimple --workdir=results_ocean --nameout=ocean_damage_{i} {s} 
            gams run_rice50x.gms --max_solretry=10 --mod_ocean=1  --climate=cbsimple --workdir=results_ocean --nameout=ocean_damage_pulse_{i} --mod_emission_pulse=ocean_damage_{i} {s} 
        )
    """
    l.append(txt)
with open(Path.cwd() / 'Data/SCC/tmp/mc_lhs.bat', 'w') as f:
    f.write('\n'.join(l))
sample_df.reset_index().rename(columns={'index':'id'}).to_parquet(Path.cwd() / 'Data/SCC/out/lhs.parquet')
