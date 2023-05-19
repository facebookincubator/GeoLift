/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';


const features = [
  {
    title: 'L\'Oreal: Full-Funnel & Omni-Channel Measurement with GeoLift for Vichy',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/Vichy_logo.jpg?raw=true',
    description: (
      <>

The French personal care company partnered with Meta to launch a GeoLift study to measure the true incremental impact
that Facebook and Instagram campaigns had on the launch of one of Vichy's newest products in Mexico. These campaigns
resulted in a 19% Lift on offline sales which translated to a 2.6x incremental Return on Investment (iROAS). Moreover,
by layering a <a href="https://www.facebook.com/business/help/1693381447650068?id=546437386202686">Meta Brand Lift Study</a> to the
GeoLift, L'Oreal was also able to measure the campaign's incremental effect on brand metrics, which had a 3% Lift
on Ad Recall!
<br></br>

        <a href="https://www.facebook.com/business/success/vichy-mexico?locale=es_LA">
        - Learn More -
        </a>
      </>
    ),
  },
  {
    title: 'Liverpool: Measuring the Long-Term Effect of Branding with GeoLift',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/liverpool_pocket.png?raw=true',
    description: (
      <>

In order to understand the impact that their branding campaigns on Facebook and Instagram had on omni-channel sales,
Liverpool executed the first-ever long-term GeoLift for upper-funnel campaigns. Through this experiment, they were
able to confirm and quantify the importance that a correct branding execution has to improve their Sales and overall
Advertising Return On Investment (ROI) in the medium and long term!
<br></br>

        <a href="https://www.facebook.com/business/success/5-liverpool?locale=es_LA">
        - Learn More -
        </a>
      </>
    ),
  },
  {
    title: 'GOAT: Measuring App Instals and In-App Purchases with GeoLift',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/GOAT_logo.jpg?raw=true',
    description: (
      <>
 GOAT, a leading technology platform for authentic sneakers, apparel, and accessories from contemporary and luxury brands,
 wanted to boost installs of its App, drive purchases at the most efficient possible cost, and gain better insights into
 the true value of its ad campaigns on Facebook and Instagram. However, changes in the ads ecosystem  made it harder to
 measure the value of its marketing efforts. To overcome these challenges, GOAT leveraged a GeoLift study. The results
 showed that Meta ads drove an incremental sales lift of 3.2% (4x iROAS)!<br></br>

        <a href="https://www.facebook.com/business/success/goat">
        - Learn More -
        </a>
      </>
    ),
  },
  {
    title: 'Measuring the Addition of Upper Funnel Optimization for Mixtiles with GeoLift',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/MixtilesLogo.png?raw=true',
    description: (
      <>
        Israel-based direct-to-consumer home design startup Mixtiles leveraged GeoLift to measure a full-funnel approach
        by using a combination of reach/upper funnel and conversion optimization campaigns. The test results showed that
        a full-funnel approach had 10% lift in new users’ purchases, 4 million net-new reach, 9% reduction in CPIC, and
        all a CPA on par with business-as-usual costs.<br></br>

        <a href="https://www.facebook.com/business/measurement/case-studies/mixtiles">
        - Learn More -
        </a>
      </>
    ),
  },
  {
    title: 'How MWMOvercame Signal Loss with GeoLift',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/mwmLogo.png?raw=true',
    description: (
      <>
        Due to iOS changes, the world’s leading creative apps publisher MWM no longer had visibility into the incremental
        impact of Facebook and Instagram campaigns on its business. GeoLift was chosen to measure the incremental free trials
        caused by Meta campaigns.Results showed that Facebook and Instagram campaigns generated 17% of incremental free trials.
      </>
    ),
  },
  {
    title: 'In-Store Sales Lift and iROAS Measurement for Sierra Nevada with GeoLift',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/SierraNevada2.png?raw=true',
    description: (
      <>
        Sierra Nevada Brewing Company leveraged Meta’s GeoLift solution to measure the lift in their In-Store Sales,
        driven by Meta marketing campaigns, revealing a 4.2% incremental sales lift that translates to an iROAS of 4.8x
        in the set of test markets included in the study.

        <a href="https://www.facebook.com/business/success/sierra-nevada-brewing-co">
        - Learn More -
        </a>
      </>
    ),
  },
  {
    title: 'Redefining App Measurement for the Gaming Industry through Meta Open Source Solutions',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/robyn_geolift.png?raw=true',
    description: (
      <>
        Flow, FVR, GameReach, Gaminator and BlueCasino - mobile gaming developers and industry leaders - sought
        to find a new source-of-truth for their last-touch attribution models after seeing a sharp decline in
        attributed downloads due to Apple's iOS 14.5+. Leveraging Meta Open Source Solutions,
        they combined the cross-channel measurement capabilities of Marketing Mix Models with&nbsp;
        <a href = "https://facebookexperimental.github.io/Robyn/">Robyn </a> and GeoLift's incrementality-based measurement
        to increase the incremental ROAS of Meta by 47% and optimize their marketing spend with a
        22% uplift in Meta's share of wallet.<br></br>
        <a href="https://www.facebook.com/business/measurement/case-studies/unified-measurement-for-app-based-businesses">
        - Learn More -
        </a> <br></br><br></br>
      </>
    ),
  },
  {
    title: 'Calibrating Attribution Models with Ualá',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/uala_logo.png?raw=true',
    description: (
      <>
        Ualá was interested in understanding whether their attribution model was correctly stating the impact
        that each media channel had.  They used an&nbsp;
        <a href = "https://facebookincubator.github.io/GeoLift/blog/inverse-geolift/">
          Inverse GeoLift </a>
        to determine the effect that their biggest online media had, and compare it to
        their attribution. Results showed that they were overattributing by 2x!<br></br>
        <a href="https://medium.com/ual%C3%A1-tech/marketing-y-ciencia-c%C3%B3mo-evaluar-el-impacto-de-campa%C3%B1as-con-geo-experimentos-ef666fef5af0">
        -  Learn More -
        </a>
      </>
    ),
  },
	{
    title: 'How Fashion Retailer Missguided Improved their Analytics with GeoLift',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/missguided_logo.png?raw=true',
    description: (
      <>
      	UK-based online fashion retailer Missguided leveraged GeoLift to measure the true incremental effect of their
        Meta marketing campaigns. Through this geo-experiment, Missguided realized that Meta was driving nearly
        double the value previously identified! The test results showed a 4.1x incremental return on ad-spend compared
        with 1.7x reported by last-click attribution models.<br></br>
        <a href="https://www.facebook.com/business/measurement/case-studies/missguided">
        - Learn More -
        </a>
      </>
    ),
  },
  {
    title: 'Proving Ad Effectiveness in Expanded Audiences with Clorox',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/clorox.png?raw=true',
    description: (
      <>
        Clorox usually runs ads to their female audience in Chile.  They ran a GeoLift to determine the
        impact that showing ads had on the extended population, comparing against women only, and measuring
        over sales.  Results showed an increment in 9% in total sales in the treatment regions.<br></br>
        <a href="https://www.facebook.com/business/success/clorox-chile">
        - Learn More -
        </a>
      </>
    ),
  },
  {
    title: 'Strategy Optimization with Multi-Cell GeoLifts for Ruffles',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/RufflesLogo.png?raw=true',
    description: (
      <>
        Ruffles ran the first multi-cell GeoLift test in Mexico to measure and optimize the impact that
        their branding campaigns had on Sales. The test results showed that broad targeting strategies
        had a 45% higher incremental Return on Investment (3.2X iROAS for Broad Audiences vs. 2.2X iROAS
        for Narrow Audiences).<br></br>
        <a href="https://www.facebook.com/business/success/ruffles-mexico?locale=es_LA">
        -  Learn More -
        </a><br></br><br></br>
      </>
    ),
  },
  {
    title: 'Cross-Channel Measurement with Liverpool',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/LiverpoolLogo.jpeg?raw=true',
    description: (
      <>
        The mexican retailer measured the first cross-channel GeoLift  test comparing the
        omni-channel incrementality of Meta and Digital Display. The results revealed the
        importance of a robust cross-channel strategy with a 14X incremental ROAS for Meta.
        <br></br>
        <a href="https://www.facebook.com/business/success/3-liverpool?locale=es_LA">
        -  Learn More -
        </a>
      </>
    ),
  },
  {
    title: 'Building Brand and Sales with Super Coffee',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/SuperCoffeeLogo2.png?raw=true',
    description: (
      <>
        The enhanced coffee company ran Facebook video and photo ads to boost brand
        awareness and used a GeoLift study to measure in-store sales, revealing an
        11.9% in-store sales lift.<br></br>
        <a href="https://www.facebook.com/business/success/super-coffee">
        -  Learn More -
        </a>
      </>
    ),
  },
    {
    title: 'Omni-Channel Measurement for Scotiabank',
    imageUrl: 'https://i.pinimg.com/originals/3f/9c/83/3f9c8390a12bf2b96c7055169e77f333.png',
    description: (
      <>
        Scotiabank Mexico was able to measure for the first time the omni-channel
        incremental impact that their Facebook campaigns had on bank account openings.<br></br>
        <a href="https://www.facebook.com/business/success/2-scotiabank-mexico?locale=es_LA">
        -  Learn More -
        </a>
      </>
    ),
  },
	{
    title: 'Omni-Channel Efficiency with Hudsons Bay Company',
    imageUrl: 'https://upload.wikimedia.org/wikipedia/commons/0/04/Hudson%27s_Bay_Company_Official_Logo_2013.svg',
    description: (
      <>
        Using GeoLift, Hudson’s Bay was able to measure lift in online and in-store sales
        as a result of campaigns on Facebook and Instagram. The company saw a 12.9X incremental
        return on ad spend (iROAS) utilizing this approach.<br></br>
        <a href="https://www.facebook.com/business/measurement/case-studies/hudsons-bay-company#">
        -  Learn More -
        </a>
      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

export default function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`Hello from ${siteConfig.title}`}
      description="Description will go into a meta tag in <head />">
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <h1 className="hero__title">{"GeoLift Success Cases"}</h1>
          <div className={styles.buttons}>
          </div>
        </div>
      </header>
      <main>
        {features && features.length > 0 && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {features.map(({title, imageUrl, description}) => (
                  <Feature
                    key={title}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}
      </main>
    </Layout>
  );
}
