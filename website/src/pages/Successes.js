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
    title: 'In-Store Sales Lift and iROAS Measurement for Sierra Nevada with GeoLift',
    imageUrl: 'https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/SierraNevada.png?raw=true',
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
        </a>
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
        </a>
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
